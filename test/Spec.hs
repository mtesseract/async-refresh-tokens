{-# LANGUAGE OverloadedStrings #-}

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

data TokenFoo

instance IsToken TokenFoo where
  tokenScopes _ = ["foo.read", "foo.write"]

createTokenStoreFoo :: LoggingT IO (TokenStore TokenFoo)
createTokenStoreFoo = do
  tokenFoo <- newEmptyTokenStore (Proxy :: Proxy TokenFoo)
  let conf = defaultTokenConf
             & tokenConfAddRequest (RequestToken tokenFoo actionFoo)
  _ <- newTokenRefresher conf
  return tokenFoo

  where actionFoo :: (MonadIO m, IsToken t) => m (RefreshResult (Token t))
        actionFoo =
          return $ RefreshResult (Token "secret-foo-token") Nothing

oneTimeRefresh :: IO ()
oneTimeRefresh = runStderrLoggingT $ do
  tokenFoo <- createTokenStoreFoo
  liftIO $ threadDelay (10 ^ 6 + 10 ^ 5)
  (Right token) <- liftIO . atomically $ readTVar tokenFoo
  liftIO $ token @?= Token "secret-foo-token"

main :: IO ()
main = do
  putStrLn ""
  defaultMain tests

  where tests = [ testGroup "Test Suite" [ testCase "Simple one-time refreshing" oneTimeRefresh ] ]
