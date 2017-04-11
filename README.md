# async-refresh-tokens

About
=====

This is Haskell library built on top of the async-refresh package
implementing the logic for refreshing of expiring access tokens.

Usage
=====

- Create new token types. Using the `DataKinds` extension we can do
  this via `data MyAppTokens = TokenFoo | TokenBar`.

- Make the tokens be instances of the `IsToken` type classes by
  defining the `tokenScopes` method and (optionally) `tokenName` (a
  human readable label for this token).

- Create new token stores (which are basically `TVar's containing the
  tokens wrapped in `Either SomeException`) using
  `newEmptyTokenStore`.

- Create a new configuration by adjusting `defaultTokenConf` using the
  functions `tokenConfAddRequest` and `tokenConfSetFactor`. The
  function `tokenConfAddRequest` expects values of type `RequestToken`
  — these values encapsulate the token stores together with a
  token-refreshing action.

- Use `newTokenRefresher` to initiate token refreshing for each
  registered token refreshing request.

Example
=======

```
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PolyKinds           #-}

data MyAppTokens = TokenFoo | TokenBar

instance IsToken 'TokenFoo where
  tokenScopes _ = ["foo.read", "foo.write"]

createTokenStoreFoo :: IO (TokenStore 'TokenFoo)
createTokenStoreFoo = runStderrLoggingT $ do
  tokenFoo <- newEmptyTokenStore (Proxy :: Proxy 'TokenFoo)
  let conf = defaultTokenConf
             & tokenConfAddRequest (RequestToken tokenFoo actionFoo)
  _ <- newTokenRefresher conf
  return tokenFoo

  where actionFoo :: (MonadIO m, IsToken t) => m (RefreshResult (Token t))
        actionFoo =
          return $ RefreshResult (Token "secret-foo-token") Nothing
```
