module Environment
  ( Environment(..)
  , lookupVar
  , new
  , assign
  ) where

import Expression ( Value(..) )
import qualified Data.Map as Map

type Environment = Map.Map String Value

new :: Environment
new = Map.empty

lookupVar :: String -> Environment -> Maybe Value
lookupVar = Map.lookup

assign :: String -> Value -> Environment -> Environment
assign = Map.insert
