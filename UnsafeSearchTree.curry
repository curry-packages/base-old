------------------------------------------------------------------------------
--- This library defines a representation of a search space as
--- a tree and various search strategies on this tree.
--- This module implements **strong encapsulation** as discussed in
--- [this paper](http://www.informatik.uni-kiel.de/~mh/papers/JFLP04_findall.html)
---
--- Warning: In contrast to the SearchTree Module, free variables that
---          are not bound in the encapsulated expression remain free!
---          This may lead to non-determinism if such an escaped
---          variable is bound later via pattern matching.
---
--- @author  Michael Hanus, Bjoern Peemoeller, Fabian Reck
--- @version July 2013
------------------------------------------------------------------------------

module UnsafeSearchTree
  ( SearchTree (..), someSearchTree, getSearchTree
  , isDefined, showSearchTree, searchTreeSize, isVar, getVarId
  , Strategy, dfsStrategy, bfsStrategy, idsStrategy, idsStrategyWith
  , allValuesDFS, allValuesBFS, allValuesIDS, allValuesIDSwith
  , ValueSequence, vsToList
  , getAllValuesWith, someValue, someValueWith
  ) where

import ValueSequence

--- A search tree is a value, a failure, or a choice between two search trees.
data SearchTree a = Value a
                  | Fail Int
                  | Or (SearchTree a) (SearchTree a)


--- Tests whether the argument is a free variable
--- This function is only useful when applied to
--- a part of a result of a encapsulated expression
--- if the argument stems from a Value-Node of
--- a SearchTree

isVar :: a -> Bool
isVar x = maybe False (const True) (getVarId x)

--- Returns the unique identifier of a free variable
--- or Nothing, if the argument was not a free variable.
--- This function is only useful when applied to
--- a part of a result of a encapsulated expression
--- if the argument stems from a Value-Node of
--- a SearchTree
getVarId :: a -> Maybe Int
getVarId external

type Strategy a = SearchTree a -> ValueSequence a

--- Returns the search tree for some expression.
getSearchTree :: a -> IO (SearchTree a)
getSearchTree x = return (someSearchTree x)

--- Internal operation to return the search tree for some expression.
--- Note that this operation is not purely declarative since
--- the ordering in the resulting search tree depends on the
--- ordering of the program rules.
someSearchTree :: a -> SearchTree a
someSearchTree external

--- Returns True iff the argument is is defined, i.e., has a value.
isDefined :: a -> Bool
isDefined x = hasValue (someSearchTree x)
 where hasValue y = case y of Value _  -> True
                              Fail _   -> False
                              Or t1 t2 -> hasValue t1 || hasValue t2

--- Shows the search tree as an intended line structure
showSearchTree :: SearchTree _ -> String
showSearchTree st = showsST [] st ""
 where
  -- `showsST ctxt <SearchTree>`, where `ctxt` is a stack of boolean flags
  -- indicating whether we show the last alternative of the respective
  -- level to enable drawing aesthetical corners
  showsST ctxt (Value  a) = indent ctxt . shows a      . nl
  showsST ctxt (Fail _)   = indent ctxt . showChar '!' . nl
  showsST ctxt (Or t1 t2) = indent ctxt . showChar '?' . nl
                          . showsST (False : ctxt) t1
                          . showsST (True  : ctxt) t2

  indent []     = id
  indent (i:is) = showString (concatMap showIndent $ reverse is)
                . showChar   (if i then llc else lmc)
                . showString (hbar : " ")
    where showIndent isLast = (if isLast then ' ' else vbar) : "  "

  vbar = '\x2502' -- vertical bar
  hbar = '\x2500' -- horizontal bar
  llc  = '\x2514' -- left lower corner
  lmc  = '\x251c' -- left middle corner

  nl           = showChar '\n'
  shows x      = showString (show x)
  showChar c   = (c:)
  showString s = (s++)

-- showSearchTree st = showST 0 st ""
--  where
--   showST _ (Value a)  = showString "Value: " . shows a . nl
--   showST _ Fail       = showString "Fail"    . nl
--   showST i (Or t1 t2) = showString "Or "
--                       . showST i' t1 . tab i' . showST i' t2
--     where i'    = i + 1
--           tab j = showString $ replicate (3 * j) ' '


--- Return the size (number of Value/Fail/Or nodes) of the search tree
searchTreeSize :: SearchTree _ -> (Int, Int, Int)
searchTreeSize (Value _)  = (1, 0, 0)
searchTreeSize (Fail _)   = (0, 1, 0)
searchTreeSize (Or t1 t2) = let (v1, f1, o1) = searchTreeSize t1
                                (v2, f2, o2) = searchTreeSize t2
                             in (v1 + v2, f1 + f2, o1 + o2 + 1)

--- Return all values in a search tree via depth-first search
allValuesDFS :: SearchTree a -> [a]
allValuesDFS = vsToList . dfsStrategy 

dfsStrategy :: Strategy a
dfsStrategy (Fail d)  = failVS d
dfsStrategy (Value x) = addVS x emptyVS
dfsStrategy (Or x y)  = dfsStrategy x |++| dfsStrategy y

--- Return all values in a search tree via breadth-first search
allValuesBFS :: SearchTree a -> [a]
allValuesBFS t = vsToList (bfsStrategy t)

children :: [SearchTree a] -> [SearchTree a]
children []             = []
children (Fail _:ts)    = children ts
children (Value _:ts)   = children ts
children (Or x y:ts)    = x:y:children ts

values :: [SearchTree a] -> ValueSequence a
values []           = emptyVS
values (Fail d:ts)  = failVS d |++| values ts
values (Value x:ts) = addVS x (values ts)
values (Or _ _:ts)  = values ts

allBFS :: [SearchTree a] -> ValueSequence a
allBFS []     = emptyVS
allBFS (t:ts) = values (t:ts) |++| allBFS (children (t:ts))

bfsStrategy :: Strategy a
bfsStrategy t = allBFS [t]



--- The default initial search depth for IDS
defIDSDepth :: Int
defIDSDepth = 100

--- The default increasing function for IDS
defIDSInc :: Int -> Int
defIDSInc = (2*)

--- Return all values in a search tree via iterative-deepening search.
allValuesIDS :: SearchTree a -> [a]
allValuesIDS t = allValuesIDSwith defIDSDepth defIDSInc t

idsStrategy :: Strategy a
idsStrategy t = idsStrategyWith defIDSDepth defIDSInc t

--- Return the list of all values in a search tree via iterative-deepening search.
--- The first argument is the initial depth bound and
--- the second argument is a function to increase the depth in each
--- iteration.
allValuesIDSwith :: Int -> (Int -> Int) -> SearchTree a -> [a]
allValuesIDSwith initdepth incrdepth = vsToList . idsStrategyWith initdepth incrdepth

--- Return all values in a search tree via iterative-deepening search.
--- The first argument is the initial depth bound and
--- the second argument is a function to increase the depth in each
--- iteration.
idsStrategyWith :: Int -> (Int -> Int) -> Strategy a
idsStrategyWith initdepth incrdepth st =
  iterIDS initdepth (collectInBounds 0 initdepth st)
 where
  iterIDS _ Nil = emptyVS
  iterIDS n (Cons x xs) = addVS x (iterIDS n xs)
  iterIDS n (FCons fd xs) = failVS fd |++| iterIDS n xs
  iterIDS n Abort = let newdepth = incrdepth n
                     in iterIDS newdepth (collectInBounds n newdepth st)

-- Collect solutions within some level bounds in a tree.
collectInBounds :: Int -> Int -> SearchTree a -> AbortList a
collectInBounds oldbound newbound st = collectLevel newbound st
 where
  collectLevel d (Fail fd)  = if d <newbound-oldbound then FCons fd Nil else Nil
  collectLevel d (Value x) = if d<newbound-oldbound then Cons x Nil else Nil
  collectLevel d (Or x y)  =
    if d>0 then concA (collectLevel (d-1) x) (collectLevel (d-1) y)
           else Abort

-- List containing "aborts" are used to implement the iterative
-- depeening strategy:

data AbortList a = Nil | Cons a (AbortList a) | FCons Int (AbortList a) | Abort

-- Concatenation on abort lists where aborts are moved to the right.
concA :: AbortList a -> AbortList a -> AbortList a
concA Abort       Abort = Abort
concA Abort       Nil = Abort
concA Abort       (Cons x xs) = Cons x (concA Abort xs)
concA Abort       (FCons d xs) = FCons d (concA Abort xs)
concA Nil         ys = ys
concA (Cons x xs) ys = Cons x (concA xs ys)
concA (FCons d xs) ys = FCons d (concA xs ys)


--- Gets all values of an expression w.r.t. a search strategy.
--- A search strategy is an operation to traverse a search tree
--- and collect all values, e.g., 'dfsStrategy' or 'bfsStrategy'.
--- Conceptually, all values are computed on a copy of the expression,
--- i.e., the evaluation of the expression does not share any results.
--- Moreover, the evaluation suspends as long as the expression
--- contains unbound variables.
getAllValuesWith :: Strategy a -> a -> IO [a]
getAllValuesWith strategy exp = do
  t <- getSearchTree exp
  return (vsToList (strategy t))


--- Returns some value for an expression.
---
--- Note that this operation is not purely declarative since
--- the computed value depends on the ordering of the program rules.
--- Thus, this operation should be used only if the expression
--- has a single value. It fails if the expression has no value.
someValue :: a -> a
someValue = someValueWith bfsStrategy

--- Returns some value for an expression w.r.t. a search strategy.
--- A search strategy is an operation to traverse a search tree
--- and collect all values, e.g., 'dfsStrategy' or 'bfsStrategy'.
---
--- Note that this operation is not purely declarative since
--- the computed value depends on the ordering of the program rules.
--- Thus, this operation should be used only if the expression
--- has a single value. It fails if the expression has no value.
someValueWith :: Strategy a -> a -> a
someValueWith strategy x = head (vsToList (strategy (someSearchTree x)))
