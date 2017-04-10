{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoImplicitPrelude         #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE PolyKinds                 #-}
{-# LANGUAGE ScopedTypeVariables       #-}


module Control.Concurrent.Async.Refresh.Tokens
  ( IsToken(..)
  , Token(..)
  , TokenRefresher
  , RequestToken(..)
  , RefreshResult(..)
  , TokenStore
  , TokenConf
  , newTokenRefresher
  , newTokenStore
  , defaultTokenConf
  , tokenConfSetFactor
  , tokenConfAddRequest
  ) where

import           ClassyPrelude
import           Control.Concurrent.Async.Lifted.Safe           (waitAny)
import           Control.Concurrent.Async.Refresh
import           Control.Concurrent.Async.Refresh.Tokens.Conf
import qualified Control.Concurrent.Async.Refresh.Tokens.Lenses as Lens
import           Control.Concurrent.Async.Refresh.Tokens.Types
import           Control.Lens
import           Control.Monad.Logger
import           Formatting

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

tokenStoreCallback :: forall m t.
                      (MonadIO m, IsToken t, MonadLogger m)
                   => TokenStore t
                   -> Either SomeException (RefreshResult (Token t)) -> m ()
tokenStoreCallback _ (Left exn) =
  logErrorN $ sformat ("Token refresh action failed: " % stext) (tshow exn)
tokenStoreCallback store res@(Right t) = do
  logDebugN $ sformat ("Token refresh action succeeded: " % stext) (tshow t)
  atomically $ writeTVar store (refreshResult <$> res)

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

newTokenStore :: (MonadIO m, IsToken t)
              => proxy t -> m (TVar (Either SomeException (Token t)))
newTokenStore _ = atomically $
  newTVar (Left (toException (TokenNotFound "")))
