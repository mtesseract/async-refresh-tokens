{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TemplateHaskell        #-}

{-|
Module      : Control.Concurrent.Async.Refresh.Tokens.Lenses
Description : This module defines lenses used within the async-refresh-tokens package.
Copyright   : (c) Moritz Schulte, 2017
License     : BSD3
Maintainer  : mtesseract@silverratio.net
Stability   : experimental
Portability : POSIX
-}

module Control.Concurrent.Async.Refresh.Tokens.Lenses where

import           Control.Lens
import           Control.Concurrent.Async.Refresh.Tokens.Types

makeFields ''TokenConf
