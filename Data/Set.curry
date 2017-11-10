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
fromList         :: Ord key => [key] -> FiniteSet key
null             :: Set _ -> Bool
member           :: Ord key => key -> FiniteSet key -> Bool
difference       :: Ord key => Set key -> Set key -> Set key
toList           :: Set key -> [key]
union            :: Ord key => Set key -> Set key -> Set key

empty = Map.empty
fromList xs = Map.fromList [ (x, ()) | x <- xs]
null = Map.null
member = Map.member
difference = Map.difference
toList = Map.keys
union = Map.union
