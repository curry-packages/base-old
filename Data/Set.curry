module Data.Set ( Set, null, size, fromList, empty, member
                , union, toList, difference
                )
  where

import qualified Data.Map as Map

-------------------------------------------------------------------------
--                                                                      -
--   FiniteSets---a thin veneer                                         -
--                                                                      -
-------------------------------------------------------------------------
type Set key = Map.Map key ()
empty            :: Set key
fromList         :: Ord key => [key] -> Set key
null             :: Set _ -> Bool
size             :: Set _ -> Int
member           :: Ord key => key -> Set key -> Bool
difference       :: Ord key => Set key -> Set key -> Set key
toList           :: Set key -> [key]
union            :: Ord key => Set key -> Set key -> Set key

empty = Map.empty
fromList xs = Map.fromList [ (x, ()) | x <- xs]
null = Map.null
size = Map.size
member = Map.member
difference = Map.difference
toList = Map.keys
union = Map.union
