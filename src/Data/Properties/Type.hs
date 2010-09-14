module Data.Properties.Type where

import Control.Applicative ((<|>))
import Data.Maybe (fromMaybe)
import qualified Data.Map as M
import qualified Data.Set as S

-- I'm deliberately not providing any analogues of java.util.Properties
-- methods inherited from HashTable or Dictionary, as the inheritance is
-- rather poorly considered to begin with.
-- None of the things in the "defaults" object are reflected by any of the
-- Hashtable-inherited methods.  While it is useful to have access to a 
-- separate collection of local bindings, IMO the top-level object itself 
-- ought to "be" the whole collection, defaults and all.

-- Also note that the java class doesn't provide any (public) way to 
-- delete a key that has a default value, nor to revert a key to its 
-- default value.  Because that capability can still be grafted in by
-- subclassing Properties, I have elected to export the Properties type
-- constructor to give users of this module the same level of 
-- flexibility at about the same level of \"fugliness\".

data Properties = Properties
    { propDefaults  :: !(Maybe Properties)
    , propLocals    :: M.Map String String
    } deriving (Eq, Ord, Show)

newProperties :: Properties
newProperties = Properties Nothing M.empty

newPropertiesWithDefaults :: Properties -> Properties
newPropertiesWithDefaults d = Properties (Just d) M.empty

getProperty :: String -> Properties -> Maybe String
getProperty key (Properties mbDef locals) 
    = M.lookup key locals
     <|> (mbDef >>= getProperty key)

getPropertyWithDefault :: String -> String -> Properties -> String
getPropertyWithDefault key def props = fromMaybe def (getProperty key props)

setProperty :: String -> String -> Properties -> Properties
setProperty key value props = props{ propLocals = M.insert key value (propLocals props) }

propertyNames :: Properties -> S.Set String
propertyNames = M.keysSet . toMap

toMap :: Properties -> M.Map String String
toMap (Properties mbDefaults locals) = M.union locals (maybe M.empty toMap mbDefaults)

