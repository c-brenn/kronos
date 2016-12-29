module Environment
  ( Environment(..)
  , assign
  , history
  , lookupVar
  , new
  , rollback
  ) where

import Expression ( Value(..) )
import qualified Data.Map as Map

type Environment = Map.Map String [Value]

new :: Environment
new = Map.empty

lookupVar :: String -> Environment -> Maybe Value
lookupVar name env =
  case Map.lookup name env of
    Just (x:xs) -> Just x
    _           -> Nothing

history :: String -> Environment -> Maybe [Value]
history = Map.lookup

assign :: String -> Value -> Environment -> Environment
assign name value = Map.insertWith (++) name [value]

rollback :: String -> Environment -> Environment
rollback = Map.update dropHead
  where dropHead [] = Nothing
        dropHead xs = Just $ drop 1 xs
