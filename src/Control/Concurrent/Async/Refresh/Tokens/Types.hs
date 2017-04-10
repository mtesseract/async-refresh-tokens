{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE OverloadedLists           #-}
{-# LANGUAGE PolyKinds                 #-}
{-# LANGUAGE Rank2Types                #-}
{-# LANGUAGE TypeFamilies              #-}

module Control.Concurrent.Async.Refresh.Tokens.Types where

import           ClassyPrelude
import           Control.Concurrent.Async.Refresh
import           Data.Proxy

data TokenException = TokenNotFound Text
 deriving (Typeable, Show)

instance Exception TokenException

-- | Type containing a token. 't' is a phantom type, which can be used
-- for making different tokens distinguishable at the type level.
newtype Token t = Token { unToken :: ByteString } deriving (Show, Eq)

data TokenConf m =
  TokenConf { _tokenConfRefreshTimeFactor :: Double
            , _tokenConfRequests          :: [RequestToken m] }

type TokenStore t = TVar (Either SomeException (Token t))

class IsToken t where
  tokenScopes :: proxy t -> [Text]
  tokenName :: proxy t -> Text
  tokenName _ = tshow (Proxy :: Proxy t)
  tokenType :: Proxy t
  tokenType = Proxy

data RequestToken m = forall t. IsToken t
  => RequestToken { requestTokenStore  :: TokenStore t
                  , requestTokenAction :: m (RefreshResult (Token t)) }

data TokenRefresher = TokenRefresher (Async ())
