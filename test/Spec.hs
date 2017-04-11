{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds         #-}

module Main where

import           Control.Concurrent                      (threadDelay)
import           Control.Concurrent.Async.Refresh.Tokens
import           Control.Concurrent.STM
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Data.Function                           ((&))
import           Data.Proxy
import           Test.Framework                          (defaultMain,
                                                          testGroup)
import           Test.Framework.Providers.HUnit          (testCase)
import           Test.HUnit                              ((@?=))

data TestTokens = TokenFoo | TokenBar

instance IsToken 'TokenFoo where
  tokenScopes _ = ["foo.read"]

oneTimeRefresh :: IO ()
oneTimeRefresh = runStderrLoggingT $ do
  tokenFoo <- newEmptyTokenStore (Proxy :: Proxy 'TokenFoo)
  let conf = defaultTokenConf
             & tokenConfAddRequest (RequestToken tokenFoo actionFoo)
  _ <- newTokenRefresher conf
  liftIO $ threadDelay (10 ^ 6 + 10 ^ 5)
  (Right token) <- liftIO . atomically $ readTVar tokenFoo
  liftIO $ token @?= Token "foo"

  where actionFoo :: (MonadIO m, IsToken t) => m (RefreshResult (Token t))
        actionFoo =
          return $ RefreshResult (Token "foo") Nothing

main :: IO ()
main = do
  putStrLn ""
  defaultMain tests

  where tests = [ testGroup "Test Suite" [ testCase "Simple one-time refreshing" oneTimeRefresh ] ]
