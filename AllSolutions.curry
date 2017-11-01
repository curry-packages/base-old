------------------------------------------------------------------------------
--- This module contains a collection of functions for
--- obtaining lists of solutions to constraints.
--- These operations are useful to encapsulate
--- non-deterministic operations between I/O actions in
--- order to connect the worlds of logic and functional programming
--- and to avoid non-determinism failures on the I/O level.
---
--- In contrast the "old" concept of encapsulated search
--- (which could be applied to any subexpression in a computation),
--- the operations to encapsulate search in this module
--- are I/O actions in order to avoid some anomalities
--- in the old concept.
---
--- @category general
------------------------------------------------------------------------------
{-# LANGUAGE CPP #-}

module AllSolutions
  ( getAllValues, getAllSolutions, getOneValue, getOneSolution
  , getAllFailures
#ifdef __PAKCS__
  , getSearchTree, SearchTree(..)
#endif
  )  where

#ifdef __PAKCS__
import Findall
#else
import SearchTree
#endif

--- Gets all values of an expression (currently, via an incomplete
--- depth-first strategy). Conceptually, all values are computed
--- on a copy of the expression, i.e., the evaluation of the expression
--- does not share any results. Moreover, the evaluation suspends
--- as long as the expression contains unbound variables.
getAllValues :: a -> IO [a]
#ifdef __PAKCS__
getAllValues e = return (findall (=:=e))
#else
getAllValues e = getSearchTree e >>= return . allValuesDFS
#endif

--- Gets one value of an expression (currently, via an incomplete
--- left-to-right strategy). Returns Nothing if the search space
--- is finitely failed.
getOneValue :: a -> IO (Maybe a)
#ifdef __PAKCS__
getOneValue x = getOneSolution (x=:=)
#else
getOneValue x = do
  st <- getSearchTree x
  let vals = allValuesDFS st
  return (if null vals then Nothing else Just (head vals))
#endif

--- Gets all solutions to a constraint (currently, via an incomplete
--- depth-first left-to-right strategy). Conceptually, all solutions
--- are computed on a copy of the constraint, i.e., the evaluation
--- of the constraint does not share any results. Moreover, this
--- evaluation suspends if the constraints contain unbound variables.
--- Similar to Prolog's findall.
getAllSolutions :: (a->Bool) -> IO [a]
#ifdef __PAKCS__
getAllSolutions c = return (findall c)
#else
getAllSolutions c = getAllValues (let x free in (x,c x)) >>= return . map fst
#endif

--- Gets one solution to a constraint (currently, via an incomplete
--- left-to-right strategy). Returns Nothing if the search space
--- is finitely failed.
getOneSolution :: (a->Bool) -> IO (Maybe a)
getOneSolution c = do
  sols <- getAllSolutions c
  return (if null sols then Nothing else Just (head sols))

--- Returns a list of values that do not satisfy a given constraint.
--- @param x - an expression (a generator evaluable to various values)
--- @param c - a constraint that should not be satisfied
--- @return A list of all values of e such that (c e) is not provable
getAllFailures :: a -> (a -> Bool) -> IO [a]
getAllFailures generator test = do
  xs <- getAllValues generator
  failures <- mapIO (naf test) xs
  return $ concat failures

-- (naf c x) returns [x] if (c x) fails, and [] otherwise.
naf :: (a -> Bool) -> a -> IO [a]
#ifdef __PAKCS__
naf c x = do
  mbl <- getOneSolution (\_->c x)
  return (maybe [x] (const []) mbl)
#else
naf c x = getOneSolution (lambda c x) >>= returner x

lambda :: (a -> Bool) -> a -> () -> Bool
lambda c x _ = c x

returner :: a -> Maybe b -> IO [a]
returner x mbl = return (maybe [x] (const []) mbl)
#endif


#ifdef __PAKCS__
--- A search tree for representing search structures.
data SearchTree a b = SearchBranch [(b,SearchTree a b)] | Solutions [a]
  deriving (Eq,Show)

--- Computes a tree of solutions where the first argument determines
--- the branching level of the tree.
--- For each element in the list of the first argument,
--- the search tree contains a branch node with a child tree
--- for each value of this element. Moreover, evaluations of
--- elements in the branch list are shared within corresponding subtrees.
getSearchTree :: [a] -> (b -> Bool) -> IO (SearchTree b a)
getSearchTree cs goal = return (getSearchTreeUnsafe cs goal)

getSearchTreeUnsafe :: [a] -> (b -> Bool) -> (SearchTree b a)
getSearchTreeUnsafe [] goal = Solutions (findall goal)
getSearchTreeUnsafe (c:cs) goal  =
                                 SearchBranch (findall (=:=(solve c cs goal)))

solve :: a -> [a] -> (b -> Bool) -> (a,SearchTree b a)
solve c cs goal | c=:=y = (y, getSearchTreeUnsafe cs goal) where y free
#endif
