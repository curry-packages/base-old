------------------------------------------------------------------------
--- This module contains an implementation of set functions.
--- The general idea of set functions is described in:
---
--- > S. Antoy, M. Hanus: Set Functions for Functional Logic Programming
--- > Proc. 11th International Conference on Principles and Practice
--- > of Declarative Programming (PPDP'09), pp. 73-82, ACM Press, 2009
---
--- Intuition: If `f` is an n-ary function, then `(setn f)` is a set-valued
--- function that collects all non-determinism caused by f (but not
--- the non-determinism caused by evaluating arguments!) in a set.
--- Thus, `(setn f a1 ... an)` returns the set of all
--- values of `(f b1 ... bn)` where `b1`,...,`bn` are values
--- of the arguments `a1`,...,`an` (i.e., the arguments are
--- evaluated "outside" this capsule so that the non-determinism
--- caused by evaluating these arguments is not captured in this capsule
--- but yields several results for `(setn...)`.
--- Similarly, logical variables occuring in `a1`,...,`an` are not bound
--- inside this capsule (in PAKCS they cause a suspension until
--- they are bound).
---
--- The set of values returned by a set function is represented
--- by an abstract type 'Values' on which several operations are
--- defined in this module. Actually, it is a multiset of values,
--- i.e., duplicates are not removed.
---
--- The handling of failures and nested occurrences of set functions
--- is not specified in the previous paper. Thus, a detailed description
--- of the semantics of set functions as implemented in this library
--- can be found in the paper
---
--- > J. Christiansen, M. Hanus, F. Reck, D. Seidel:
--- > A Semantics for Weakly Encapsulated Search in Functional Logic Programs
--- > Proc. 15th International Conference on Principles and Practice
--- > of Declarative Programming (PPDP'13), pp. 49-60, ACM Press, 2013
---
--- Restrictions of the PAKCS implementation of set functions:
---
--- 1. The set is a multiset, i.e., it might contain multiple values.
--- 2. The multiset of values is completely evaluated when demanded.
---    Thus, if it is infinite, its evaluation will not terminate
---    even if only some elements (e.g., for a containment test)
---    are demanded. However, for the emptiness test, at most one
---    value will be computed
--- 3. The arguments of a set function are strictly evaluated before
---    the set functions itself will be evaluated.
---
--- @author Michael Hanus, Fabian Reck
--- @version September 2018
--- @category general
------------------------------------------------------------------------
{-# LANGUAGE CPP #-}
{-# OPTIONS_CYMAKE -Wno-incomplete-patterns #-}

module SetFunctions
  (set0, set1, set2, set3, set4, set5, set6, set7
#ifdef __PAKCS__
#else
  , set0With, set1With, set2With, set3With, set4With, set5With, set6With
  , set7With
#endif
  , Values, isEmpty, notEmpty, valueOf
  , choose, chooseValue, select, selectValue
  , mapValues, foldValues, filterValues
  , minValue, minValueBy, maxValue, maxValueBy
  , values2list, printValues, sortValues, sortValuesBy
  ) where

import Data.List   ( delete, minimum, minimumBy, maximum, maximumBy )
import Sort   ( mergeSortBy )
#ifdef __PAKCS__
import Findall
#else
import SearchTree
#endif

#ifdef __PAKCS__
------------------------------------------------------------------------
--- Combinator to transform a 0-ary function into a corresponding set function.
set0 :: b -> Values b
set0 f = Values (oneValue f) (allValues f)

--- Combinator to transform a unary function into a corresponding set function.
set1 :: (a1 -> b) -> a1 -> Values b
set1 f x | x=:=x = Values (oneValue (f x)) (allValues (f x))

--- Combinator to transform a binary function into a corresponding set function.
set2 :: (a1 -> a2 -> b) -> a1 -> a2 -> Values b
set2 f x1 x2
  | x1=:=x1 & x2=:=x2
  = Values (oneValue (f x1 x2)) (allValues (f x1 x2))

--- Combinator to transform a function of arity 3
--- into a corresponding set function.
set3 :: (a1 -> a2 -> a3 -> b) -> a1 -> a2 -> a3 -> Values b
set3 f x1 x2 x3
  | x1=:=x1 & x2=:=x2 & x3=:=x3
  = Values (oneValue (f x1 x2 x3)) (allValues (f x1 x2 x3))

--- Combinator to transform a function of arity 4
--- into a corresponding set function.
set4 :: (a1 -> a2 -> a3 -> a4 -> b) -> a1 -> a2 -> a3 -> a4 -> Values b
set4 f x1 x2 x3 x4
  | x1=:=x1 & x2=:=x2 & x3=:=x3 & x4=:=x4
  = Values (oneValue (f x1 x2 x3 x4)) (allValues (f x1 x2 x3 x4))

--- Combinator to transform a function of arity 5
--- into a corresponding set function.
set5 :: (a1 -> a2 -> a3 -> a4 -> a5 -> b)
      -> a1 -> a2 -> a3 -> a4 -> a5 -> Values b
set5 f x1 x2 x3 x4 x5
  | x1=:=x1 & x2=:=x2 & x3=:=x3 & x4=:=x4 & x5=:=x5
  = Values (oneValue (f x1 x2 x3 x4 x5)) (allValues (f x1 x2 x3 x4 x5))

--- Combinator to transform a function of arity 6
--- into a corresponding set function.
set6 :: (a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> b)
      -> a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> Values b
set6 f x1 x2 x3 x4 x5 x6
  | x1=:=x1 & x2=:=x2 & x3=:=x3 & x4=:=x4 & x5=:=x5 & x6=:=x6
  = Values (oneValue (f x1 x2 x3 x4 x5 x6))
           (allValues (f x1 x2 x3 x4 x5 x6))

--- Combinator to transform a function of arity 7
--- into a corresponding set function.
set7 :: (a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> a7 -> b)
      -> a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> a7 -> Values b
set7 f x1 x2 x3 x4 x5 x6 x7
  | x1=:=x1 & x2=:=x2 & x3=:=x3 & x4=:=x4 & x5=:=x5 & x6=:=x6 & x7=:=x7
  = Values (oneValue (f x1 x2 x3 x4 x5 x6 x7))
           (allValues (f x1 x2 x3 x4 x5 x6 x7))

------------------------------------------------------------------------
#else
------------------------------------------------------------------------
--- Combinator to transform a 0-ary function into a corresponding set function.
set0 :: b -> Values b
set0 f = set0With dfsStrategy f

--- Combinator to transform a 0-ary function into a corresponding set function
--- that uses a given strategy to compute its values.
set0With :: Strategy b -> b -> Values b
set0With s f = Values (vsToList (s (someSearchTree f)))

--- Combinator to transform a unary function into a corresponding set function.
set1 :: (a1 -> b) -> a1 -> Values b
set1 f x = set1With dfsStrategy f x

--- Combinator to transform a unary function into a corresponding set function
--- that uses a given strategy to compute its values.
set1With :: Strategy b -> (a1 -> b) -> a1 -> Values b
set1With s f x = allVs s (\_ -> f x)

--- Combinator to transform a binary function into a corresponding set function.
set2 :: (a1 -> a2 -> b) -> a1 -> a2 -> Values b
set2 f x1 x2 = set2With dfsStrategy f x1 x2

--- Combinator to transform a binary function into a corresponding set function
--- that uses a given strategy to compute its values.
set2With :: Strategy b -> (a1 -> a2 -> b) -> a1 -> a2 -> Values b
set2With s f x1 x2 = allVs s (\_ -> f x1 x2)

--- Combinator to transform a function of arity 3
--- into a corresponding set function.
set3 :: (a1 -> a2 -> a3 -> b) -> a1 -> a2 -> a3 -> Values b
set3 f x1 x2 x3 = set3With dfsStrategy f x1 x2 x3

--- Combinator to transform a function of arity 3
--- into a corresponding set function
--- that uses a given strategy to compute its values.
set3With :: Strategy b -> (a1 -> a2 -> a3 -> b) -> a1 -> a2 -> a3 -> Values b
set3With s f x1 x2 x3 = allVs s (\_ -> f  x1 x2 x3)

--- Combinator to transform a function of arity 4
--- into a corresponding set function.
set4 :: (a1 -> a2 -> a3 -> a4 -> b) -> a1 -> a2 -> a3 -> a4 -> Values b
set4 f x1 x2 x3 x4 = set4With dfsStrategy f x1 x2 x3 x4

--- Combinator to transform a function of arity 4
--- into a corresponding set function
--- that uses a given strategy to compute its values.
set4With :: Strategy b -> (a1 -> a2 -> a3 -> a4 -> b) -> a1 -> a2 -> a3 -> a4
         -> Values b
set4With s f x1 x2 x3 x4 = allVs s (\_ -> f x1 x2 x3 x4)

--- Combinator to transform a function of arity 5
--- into a corresponding set function.
set5 :: (a1 -> a2 -> a3 -> a4 -> a5 -> b)
      -> a1 -> a2 -> a3 -> a4 -> a5 -> Values b
set5 f x1 x2 x3 x4 x5 = set5With dfsStrategy f x1 x2 x3 x4 x5

--- Combinator to transform a function of arity 5
--- into a corresponding set function
--- that uses a given strategy to compute its values.
set5With :: Strategy b -> (a1 -> a2 -> a3 -> a4 -> a5 -> b)
         -> a1 -> a2 -> a3 -> a4 -> a5 -> Values b
set5With s f x1 x2 x3 x4 x5 = allVs s (\_ -> f x1 x2 x3 x4 x5)

--- Combinator to transform a function of arity 6
--- into a corresponding set function.
set6 :: (a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> b)
      -> a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> Values b
set6 f x1 x2 x3 x4 x5 x6 = set6With dfsStrategy f x1 x2 x3 x4 x5 x6

--- Combinator to transform a function of arity 6
--- into a corresponding set function
--- that uses a given strategy to compute its values.
set6With :: Strategy b -> (a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> b)
         -> a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> Values b
set6With s f x1 x2 x3 x4 x5 x6 =
 allVs s (\_ -> f x1 x2 x3 x4 x5 x6)

--- Combinator to transform a function of arity 7
--- into a corresponding set function.
set7 :: (a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> a7 -> b)
      -> a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> a7 -> Values b
set7 f x1 x2 x3 x4 x5 x6 x7 = set7With dfsStrategy f x1 x2 x3 x4 x5 x6 x7

--- Combinator to transform a function of arity 7
--- into a corresponding set function
--- that uses a given strategy to compute its values.
set7With :: Strategy b -> (a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> a7 -> b)
         -> a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> a7 -> Values b
set7With s f x1 x2 x3 x4 x5 x6 x7 =
 allVs s (\_ -> f x1 x2 x3 x4 x5 x6 x7)

------------------------------------------------------------------------
-- Auxiliaries:

-- Collect all values of an expression (represented as a constant function)
-- in a list:
allVs :: Strategy a -> (() -> a) -> Values a
allVs s f =
  Values (vsToList ((incDepth $!! s)
                      ((incDepth $!! someSearchTree) ((incDepth $!! f) ()))))

-- Apply a function to an argument where the encapsulation level of the
-- argument is incremented.
incDepth :: (a -> b) -> a -> b
incDepth external
------------------------------------------------------------------------
#endif

----------------------------------------------------------------------
--- Abstract type representing multisets of values.

#ifdef __PAKCS__
data Values a = Values (Maybe a) [a]
#else
data Values a = Values [a]
#endif

--- Internal operation to extract all elements of a multiset of values.
valuesOf :: Values a -> [a]
#ifdef __PAKCS__
valuesOf (Values _ s) = s
#else
valuesOf (Values s) = s
#endif

----------------------------------------------------------------------

--- Is a multiset of values empty?
isEmpty :: Values a -> Bool
#ifdef __PAKCS__
isEmpty (Values firstval _) = case firstval of Nothing -> True
                                               Just _  -> False
#else
isEmpty (Values vs) = null vs
#endif

--- Is a multiset of values not empty?
notEmpty :: Values a -> Bool
notEmpty vs = not (isEmpty vs)

--- Is some value an element of a multiset of values?
valueOf :: Eq a => a -> Values a -> Bool
valueOf e s = e `elem` valuesOf s

--- Chooses (non-deterministically) some value in a multiset of values
--- and returns the chosen value and the remaining multiset of values.
--- Thus, if we consider the operation `chooseValue` by
---
---     chooseValue x = fst (choose x)
---
--- then `(set1 chooseValue)` is the identity on value sets, i.e.,
--- `(set1 chooseValue s)` contains the same elements as the
--- value set `s`.
choose :: Eq a => Values a -> (a,Values a)
#ifdef __PAKCS__
choose (Values _ vs) =
  (x, Values (if null xs then Nothing else Just (head xs)) xs)
 where x = foldr1 (?) vs
       xs = delete x vs
#else
choose (Values vs) = (x, Values xs)
  where x = foldr1 (?) vs
        xs = delete x vs
#endif

--- Chooses (non-deterministically) some value in a multiset of values
--- and returns the chosen value.
--- Thus, `(set1 chooseValue)` is the identity on value sets, i.e.,
--- `(set1 chooseValue s)` contains the same elements as the
--- value set `s`.
chooseValue :: Eq a => Values a -> a
chooseValue s = fst (choose s)

--- Selects (indeterministically) some value in a multiset of values
--- and returns the selected value and the remaining multiset of values.
--- Thus, `select` has always at most one value.
--- It fails if the value set is empty.
---
--- **NOTE:**
--- The usage of this operation is only safe (i.e., does not destroy
--- completeness) if all values in the argument set are identical.
select :: Values a -> (a,Values a)
#ifdef __PAKCS__
select (Values _ (x:xs)) =
  (x, Values (if null xs then Nothing else Just (head xs)) xs)
#else
select (Values (x:xs)) = (x, Values xs)
#endif

--- Selects (indeterministically) some value in a multiset of values
--- and returns the selected value.
--- Thus, `selectValue` has always at most one value.
--- It fails if the value set is empty.
---
--- **NOTE:**
--- The usage of this operation is only safe (i.e., does not destroy
--- completeness) if all values in the argument set are identical.
selectValue :: Values a -> a
#ifdef __PAKCS__
selectValue (Values (Just val) _) = val
#else
selectValue s = fst (select s)
#endif

--- Maps a function to all elements of a multiset of values.
mapValues :: (a -> b) -> Values a -> Values b
#ifdef __PAKCS__
mapValues f (Values mbval s) = Values (maybe Nothing (Just . f) mbval) (map f s)
#else
mapValues f (Values s) = Values (map f s)
#endif

--- Accumulates all elements of a multiset of values by applying a binary
--- operation. This is similarly to fold on lists, but the binary operation
--- must be <b>commutative</b> so that the result is independent of the order
--- of applying this operation to all elements in the multiset.
foldValues :: (a -> a -> a) -> a -> Values a -> a
foldValues f z s = foldr f z (valuesOf s)

--- Keeps all elements of a multiset of values that satisfy a predicate.
filterValues :: (a -> Bool) -> Values a -> Values a
#ifdef __PAKCS__
filterValues p (Values _ s) = Values val xs
 where
  xs = filter p s
  val = if null xs then Nothing else Just (head xs)
#else
filterValues p (Values s) = Values (filter p s)
#endif

--- Returns the minimum of a non-empty multiset of values
--- according to the given comparison function on the elements.
minValue :: Ord a => Values a -> a
minValue s = minimum (valuesOf s)

--- Returns the minimum of a non-empty multiset of values
--- according to the given comparison function on the elements.
minValueBy :: (a -> a -> Ordering) -> Values a -> a
minValueBy cmp s = minimumBy cmp (valuesOf s)

--- Returns the maximum of a non-empty multiset of values
--- according to the given comparison function on the elements.
maxValue :: Ord a => Values a -> a
maxValue s = maximum (valuesOf s)

--- Returns the maximum of a non-empty multiset of values
--- according to the given comparison function on the elements.
maxValueBy :: (a -> a -> Ordering) -> Values a -> a
maxValueBy cmp s = maximumBy cmp (valuesOf s)

--- Puts all elements of a multiset of values in a list.
--- Since the order of the elements in the list might depend on
--- the time of the computation, this operation is an I/O action.
values2list :: Values a -> IO [a]
values2list s = return (valuesOf s)

--- Prints all elements of a multiset of values.
printValues :: Show a => Values a -> IO ()
printValues s = values2list s >>= mapIO_ print

--- Transforms a multiset of values into a list sorted by
--- the standard term ordering. As a consequence, the multiset of values
--- is completely evaluated.
sortValues :: Ord a => Values a -> [a]
sortValues = sortValuesBy (<=)

--- Transforms a multiset of values into a list sorted by a given ordering
--- on the values. As a consequence, the multiset of values
--- is completely evaluated.
--- In order to ensure that the result of this operation is independent of the
--- evaluation order, the given ordering must be a total order.
sortValuesBy :: (a -> a -> Bool) -> Values a -> [a]
sortValuesBy leq s = mergeSortBy leq (valuesOf s)

------------------------------------------------------------------------
