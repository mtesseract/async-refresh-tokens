{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoImplicitPrelude         #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE PolyKinds                 #-}
{-# LANGUAGE ScopedTypeVariables       #-}

{-|
Module      : Control.Concurrent.Async.Refresh.Tokens
Description : This module exposes the API of the async-refresh-tokens package.
Copyright   : (c) Moritz Schulte, 2017
License     : BSD3
Maintainer  : mtesseract@silverratio.net
Stability   : experimental
Portability : POSIX

The async-refresh-tokens package is built on top of the async-refresh
package and provides the core logic for renewal of expiring access
tokens according to user-provided actions.
-}

module Control.Concurrent.Async.Refresh.Tokens
  ( IsToken(..)
  , Token(..)
  , TokenRefresher
  , tokenRefresherAsync
  , RequestToken(..)
  , RefreshResult(..)
  , TokenStore
  , TokenConf
  , newTokenRefresher
  , newEmptyTokenStore
  , defaultTokenConf
  , tokenConfSetFactor
  , tokenConfAddRequest
  ) where

import           Control.Concurrent.Async.Refresh.Tokens.Prelude

import           Control.Concurrent.Async.Lifted.Safe            (cancel, waitAny)
import           Control.Concurrent.Async.Refresh
import           Control.Concurrent.Async.Refresh.Tokens.Conf
import qualified Control.Concurrent.Async.Refresh.Tokens.Lenses  as Lens
import           Control.Concurrent.Async.Refresh.Tokens.Types
import           Lens.Micro

-- | Start a new token refresher for the provided configuration.
-- Returns a 'TokenRefresher' handle representing the running token
-- refresher.
newTokenRefresher :: forall m.
                     ( MonadIO m
                     , MonadBaseControl IO m
                     , MonadMask m
                     , MonadLogger m
                     , Forall (Pure m) )
                  => TokenConf m -> m TokenRefresher
newTokenRefresher conf = do
  asyncHandle <- async $
    bracket (mapM spawnSingleTokenRefresher (conf ^. Lens.requests))
            (mapM cancel)
            waitForTermination
  return $ TokenRefresher asyncHandle
  where waitForTermination :: [Async ()] -> m ()
        waitForTermination asyncHandles = do
          void $ waitAny asyncHandles
          logErrorN "Token Refresher terminated unexpectedly"

-- | Spawn an async refresher for the provided token.
spawnSingleTokenRefresher :: forall m.
                             ( MonadIO m
                             , MonadBaseControl IO m
                             , MonadMask m
                             , MonadLogger m
                             , Forall (Pure m) )
                          => RequestToken m -> m (Async ())
spawnSingleTokenRefresher (RequestToken store action) = do
  let conf = newAsyncRefreshConf action
             & asyncRefreshConfSetCallback (tokenStoreCallback store)
  asyncRefreshAsync <$> newAsyncRefresh conf

-- | Extract the 'Async' handle from the provided 'TokenRefresher'.
tokenRefresherAsync :: TokenRefresher -> Async ()
tokenRefresherAsync (TokenRefresher asyncHandle) = asyncHandle

-- | Callback to be used by async-refresh package, which simply
-- updates the provided token store.
tokenStoreCallback :: forall m t.
                      (MonadIO m, IsToken t, MonadLogger m)
                   => TokenStore t
                   -> Either SomeException (RefreshResult (Token t)) -> m ()
tokenStoreCallback _ (Left exn) =
  logErrorN $ sformat ("Token refresh action failed: " % stext) (tshow exn)
tokenStoreCallback store res@(Right t) = do
  logDebugN $ sformat ("Token refresh action succeeded: " % stext) (tshow t)
  atomically $ writeTVar store (refreshResult <$> res)

-- | Create a new empty token store for the provided token type.
newEmptyTokenStore :: (MonadIO m, IsToken t)
                   => proxy t -> m (TVar (Either SomeException (Token t)))
newEmptyTokenStore _ = atomically $
  newTVar (Left (toException (TokenNotFound "")))
