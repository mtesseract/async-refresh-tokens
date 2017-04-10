{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           ClassyPrelude
import           Control.Concurrent.Async.Refresh.Tokens
import           Control.Monad.Logger
import           Data.Function                           ((&))
import           Data.Proxy
import           Test.Framework                          (defaultMain, testGroup)
import           Test.Framework.Providers.HUnit          (testCase)
import           Test.HUnit                              ((@?=))

data TestTokens = TokenFoo | TokenBar

instance IsToken 'TokenFoo where
  tokenScopes _ = ["foo.read"]

data Exn = NotFound deriving (Show, Typeable)

instance Exception Exn

oneTimeRefresh :: IO ()
oneTimeRefresh = runStderrLoggingT $ do
  tokenFoo <- newEmptyTokenStore (Proxy :: Proxy 'TokenFoo)
  let conf = defaultTokenConf
             & tokenConfAddRequest (RequestToken tokenFoo actionFoo)
  _ <- newTokenRefresher conf
  threadDelay (10 ^ 6 + 10 ^ 5)
  (Right token) <- atomically $ readTVar tokenFoo
  liftIO $ token @?= Token "foo"

  where actionFoo :: (MonadIO m, IsToken t) => m (RefreshResult (Token t))
        actionFoo =
          return $ RefreshResult (Token "foo") Nothing

main :: IO ()
main = do
  putStrLn ""
  defaultMain tests

  where tests = [ testGroup "Test Suite" [ testCase "Simple one-time refreshing" oneTimeRefresh ] ]
