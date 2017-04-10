{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TemplateHaskell        #-}

module Control.Concurrent.Async.Refresh.Tokens.Lenses where

import           Control.Lens
import           Control.Concurrent.Async.Refresh.Tokens.Types

makeFields ''TokenConf
