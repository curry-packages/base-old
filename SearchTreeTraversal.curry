------------------------------------------------------------------------------
--- Implements additional traversals on search trees.
---
--- @author Sebastian Fischer <sebf@informatik.uni-kiel.de>
--- @version February 2016
--- @category algorithm
------------------------------------------------------------------------------

module SearchTreeTraversal
  (
    depthDiag, rndDepthDiag, levelDiag, rndLevelDiag, rndLevelDiagFlat
  ) where

import List       ( diagonal )
import Random     ( nextInt, nextIntRange, shuffle )
import SearchTree

--- Splits a random seeds into new seeds.
--- The range avoids large negative seeds (which cause problems with PAKCS).
split :: Int -> [Int]
split n = nextIntRange n 2147483648

--- diagonalized depth first search.
---
--- @param t search tree
--- @return enumeration of values in given search tree
---
depthDiag :: SearchTree a -> [a]
depthDiag t = [ x | Value x <- dfsDiag t ]

dfsDiag :: SearchTree a -> [SearchTree a]
-- dfsDiag Suspend      = []
dfsDiag (Fail _)     = []
dfsDiag t@(Value _)  = [t]
dfsDiag (Or t1 t2) = diagonal (map dfsDiag [t1,t2])


--- randomized variant of diagonalized depth first search.
---
--- @param t search tree
--- @return enumeration of values in given search tree
---
rndDepthDiag :: Int -> SearchTree a -> [a]
rndDepthDiag rnd t = [ x | Value x <- rndDfsDiag rnd t ]

rndDfsDiag :: Int -> SearchTree a -> [SearchTree a]
-- rndDfsDiag _   Suspend      = []
rndDfsDiag _   (Fail _)     = []
rndDfsDiag _   t@(Value _)  = [t]
rndDfsDiag rnd (Or t1 t2) =
  diagonal (zipWith rndDfsDiag rs (shuffle r [t1,t2]))
 where
  r:rs = split rnd


--- diagonalization of devels.
---
--- @param t search tree
--- @return enumeration of values in given search tree
---
levelDiag :: SearchTree a -> [a]
levelDiag t = [ x | Value x <- diagonal (levels [t]) ]

levels :: [SearchTree a] -> [[SearchTree a]]
levels ts | null ts   = []
          | otherwise = ts : levels [ u | Or u1 u2 <- ts, u <- [u1,u2] ]


--- randomized diagonalization of levels.
---
--- @param t search tree
--- @return enumeration of values in given search tree
---
rndLevelDiag :: Int -> SearchTree a -> [a]
rndLevelDiag rnd t = [ x | Value x <- diagonal (rndLevels rnd [t]) ]

rndLevels :: Int -> [SearchTree a] -> [[SearchTree a]]
rndLevels rnd ts
  | null ts = []
  | otherwise
    = ts : rndLevels r (concat (zipWith shuffle rs [ [u1,u2] | Or u1 u2 <- ts ]))
 where
  r:rs = split rnd

--- randomized diagonalization of levels with flattening.

rndLevelDiagFlat :: Int -> Int -> SearchTree a -> [a]
rndLevelDiagFlat d rnd t =
  concat $ transpose (zipWith rndLevelDiag rs (flatRep d [t]))
 where
  rs = split rnd

flat :: SearchTree a -> [SearchTree a]
flat t@(Value _) = [t]
flat (Fail _)    = [] -- pretend Fail ~ Or []
flat (Or t1 t2)  = [t1,t2]

flatRep :: Int -> [SearchTree a] -> [SearchTree a]
flatRep n ts
  | n==0      = ts
  | otherwise = flatRep (n-1) (concatMap flat ts)

-- auxiliary functions

transpose :: [[a]] -> [[a]]
transpose [] = []
transpose ([] : xss) = transpose xss
transpose ((x:xs) : xss)
  = (x : [h | (h:_) <- xss]) : transpose (xs : [t | (_:t) <- xss])
