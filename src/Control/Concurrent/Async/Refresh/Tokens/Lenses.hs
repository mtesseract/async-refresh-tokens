{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TemplateHaskell        #-}

module Control.Concurrent.Async.Refresh.Tokens.Lenses where

-- This module defines lenses used within this package.

import           Control.Lens
import           Control.Concurrent.Async.Refresh.Tokens.Types

makeFields ''TokenConf
