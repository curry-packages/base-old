------------------------------------------------------------------------------
--- This library implements some operations to generate search trees
--- for various data types.
---
--- The library also provides combinators to support the easy definition
--- of search tree generators for user-defined data types.
--- If a data type is defined by
---
---     data T a1 ... an = ... | C t1 ... tn | ....
---
--- then one can define a search tree generator for this type by
---
---     genT gena1 ... genan =
---       ... ||| genCons<n> C gen_t1 ... gen_t1 ||| ...
---
--- where `gen_ti` denotes the search tree generator for type `ti`.
--- For instance, a search tree generator for the type
---
---     data Tree a = Leaf a
---                 | Node [Tree a]
---
--- can be defined by
---
---     genTree gena =  genCons1 Leaf gena
---                 ||| genCons1 Node (genList (genTree gena))
---
--- @author  Michael Hanus
--- @version February 2017
--- @category algorithm
------------------------------------------------------------------------------

module Control.Monad.SearchTree.Generators
  ( genBool, genNat, genInt, genFloat, genChar, genList
  , genMaybe, genEither, genUnit, genPair, genTriple, genTuple4, genTuple5
  , genOrdering
  , genCons0, genCons1, genCons2, genCons3, genCons4, genCons5
  , (|||)
  ) where

import Float
import Data.Either
import Control.Monad.SearchTree
import Control.Monad.SearchTree.Traversal

infixr 1 |||

--- Replace in a given search tree each value node by a new search tree
--- according to the function provided as the second argument.
valsTo :: SearchTree a -> (a -> SearchTree b) -> SearchTree b
valsTo (Value a)  f = f a
valsTo (Fail i)   _ = Fail i
valsTo (Or t1 t2) f = Or (valsTo t1 f) (valsTo t2 f)

--- Constructs an alternative of two search trees.
(|||) :: SearchTree a -> SearchTree a -> SearchTree a
x ||| y = Or x y

--- Constructs a generator for a nullary constructor.
genCons0 :: a -> SearchTree a
genCons0 = Value

--- Constructs a generator for a unary constructor where the generator
--- for the argument type is provided.
genCons1 :: (a -> b) -> SearchTree a -> SearchTree b
genCons1 c gena = valsTo gena (Value . c)

--- Constructs a generator for a binary constructor where the generators
--- for the argument types are provided.
genCons2 :: (a1 -> a2 -> b) -> SearchTree a1 -> SearchTree a2 -> SearchTree b
genCons2 c gena1 gena2 =
  valsTo gena1 (\a1 ->
  valsTo gena2 (\a2 -> Value (c a1 a2)))

--- Constructs a generator for a ternary constructor where the generators
--- for the argument types are provided.
genCons3 :: (a1 -> a2 -> a3 -> b) -> SearchTree a1 -> SearchTree a2
         -> SearchTree a3 -> SearchTree b
genCons3 c gena1 gena2 gena3 =
  valsTo gena1 (\a1 ->
  valsTo gena2 (\a2 ->
  valsTo gena3 (\a3 -> Value (c a1 a2 a3))))

--- Constructs a generator for a constructor of arity 4 where the generators
--- for the argument types are provided.
genCons4 :: (a1 -> a2 -> a3 -> a4 -> b) -> SearchTree a1 -> SearchTree a2
         -> SearchTree a3 -> SearchTree a4 -> SearchTree b
genCons4 c gena1 gena2 gena3 gena4 =
  valsTo gena1 (\a1 ->
  valsTo gena2 (\a2 ->
  valsTo gena3 (\a3 ->
  valsTo gena4 (\a4 -> Value (c a1 a2 a3 a4)))))

--- Constructs a generator for a constructor of arity 5 where the generators
--- for the argument types are provided.
genCons5 :: (a1 -> a2 -> a3 -> a4 -> a5 -> b) -> SearchTree a1 -> SearchTree a2
         -> SearchTree a3 -> SearchTree a4 -> SearchTree a5 -> SearchTree b
genCons5 c gena1 gena2 gena3 gena4 gena5 =
  valsTo gena1 (\a1 ->
  valsTo gena2 (\a2 ->
  valsTo gena3 (\a3 ->
  valsTo gena4 (\a4 ->
  valsTo gena5 (\a5 -> Value (c a1 a2 a3 a4 a5))))))

------------------------------------------------------------------------------
-- Generators for specific types:

--- Generates a search tree for positive natural numbers:
genNat :: SearchTree Int
genNat = Or (Value 1)
            (Or (valsTo genNat (\n -> Value (2*n)))
                (valsTo genNat (\n -> Value (2*n+1))))

--- Generates a search tree for integer values.
genInt :: SearchTree Int
genInt = Or genNat
            (Or (Value 0)
                (valsTo genNat (Value . (0 -))))

--- Generates a search tree for Float values.
genFloat :: SearchTree Float
genFloat = Or genPosFloat
              (Or (Value 0.0)
                  (valsTo genPosFloat (Value . (0.0 -.))))

--- Generates a search tree for positive Float values.
genPosFloat :: SearchTree Float
genPosFloat = valsTo (genPair genNat genNat) ii2f
 where
  -- Combine two naturals to a float value:
  ii2f (x,y) = Value (i2f x +. nat2float 0.1 y)
  -- Transform an natural to float<1, e.g., nat2float 0.1 135 = 0.531
  nat2float m i =
    if i == 0 then 0.0
              else nat2float (m/.10) (i `div` 10) +. m *. i2f (i `mod` 10)

--- Generates a search tree for Boolean values.
genBool :: SearchTree Bool
genBool = Or (Value False) (Value True)

--- Generates a search tree for character values.
--- In order to obtain readable values, we only generate letters and digits.
genChar :: SearchTree Char
genChar = foldr1 Or (map Value chars)
 where
   chars = map chr ([65..90] ++ [97..122] ++ [48..57])
-- or use this for generating all characters:
-- genChar = valsTo genNat (Value . chr . (+1))

--- Generates a search tree for list values where the search tree for
--- the elements is given as a parameter.
genList :: SearchTree a -> SearchTree [a]
genList genx =
  Or (Value [])
     (genCons2 (:) genx (genList genx))

--- Generates a search tree for `Maybe` values where the search tree for
--- the possible element is given as a parameter.
genMaybe :: SearchTree a -> SearchTree (Maybe a)
genMaybe genx = Value Nothing ||| genCons1 Just genx

--- Generates a search tree for `Either` values where the search tree for
--- the possible elements is given as a parameter.
genEither :: SearchTree a -> SearchTree b -> SearchTree (Either a b)
genEither genx geny = genCons1 Left  genx ||| genCons1 Right geny

--- Generates a search tree for the unit values.
genUnit :: SearchTree ()
genUnit = Value ()

--- Generates a search tree for pair of values where the search tree generators
--- for the components types are given as parameters.
genPair :: SearchTree a -> SearchTree b -> SearchTree (a,b)
genPair = genCons2 (\x y -> (x,y))

--- Generates a search tree for triple of values where the search tree
--- generators for the components types are given as parameters.
genTriple :: SearchTree a -> SearchTree b -> SearchTree c
          -> SearchTree (a,b,c)
genTriple = genCons3 (\x y z -> (x,y,z))

--- Generates a search tree for quadruple of values where the search tree
--- generators for the components types are given as parameters.
genTuple4 :: SearchTree a -> SearchTree b -> SearchTree c -> SearchTree d
          -> SearchTree (a,b,c,d)
genTuple4 = genCons4 (\x y z1 z2 -> (x,y,z1,z2))

--- Generates a search tree for 5-tuple of values where the search tree
--- generators for the components types are given as parameters.
genTuple5 :: SearchTree a -> SearchTree b -> SearchTree c -> SearchTree d
          -> SearchTree e -> SearchTree (a,b,c,d,e)
genTuple5 = genCons5 (\x y z1 z2 z3 -> (x,y,z1,z2,z3))

--- Generates a search tree for the `Ordering` values.
genOrdering :: SearchTree Ordering
genOrdering = Or (Value LT) (Or (Value EQ) (Value GT))
