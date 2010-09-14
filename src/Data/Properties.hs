module Data.Properties
    ( Properties(..)
    , newProperties, newPropertiesWithDefaults
    , getProperty, getPropertyWithDefault
    , load, store
    , setProperty
    ) where

import Data.Properties.Type
import Data.Properties.PlainText