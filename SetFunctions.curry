------------------------------------------------------------------------
--- This module contains an implementation of set functions.
--- The general idea of set functions
--- is described in:
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
--- inside this capsule.
--- The set of values returned by a set function is represented
--- by an abstract type 'Values' on which several operations are
--- defined in this module. Actually, it is a multiset of values,
--- i.e., duplicates are not removed.
---
--- @author Michael Hanus
--- @version January 2013
------------------------------------------------------------------------

module SetFunctions
         (set0,set1,set2,set3,set4,set5,set6,set7,
          set0With,set1With,set2With,set3With,set4With,set5With,set6With,set7With,
          Values,isEmpty,valueOf,
          choose,chooseValue,select,selectValue,
          mapValues,foldValues,minValue,maxValue,
          values2list,printValues,sortValues,sortValuesBy)
 where

import Sort(mergeSort)
import SearchTree
import List(delete)

--- Combinator to transform a 0-ary function into a corresponding set function.
set0 :: b -> Values b
set0 f = set0With dfsStrategy f

set0With :: Strategy b -> b -> Values b
set0With s f = allVs s f 

--- Combinator to transform a unary function into a corresponding set function.
set1 :: (a1 -> b) -> a1 -> Values b
set1 f x = set1With dfsStrategy f x

set1With :: Strategy b -> (a1 -> b) -> a1 -> Values b
set1With s f x = allVs s (f (cover x))

--- Combinator to transform a binary function into a corresponding set function.
set2 :: (a1 -> a2 -> b) -> a1 -> a2 -> Values b
set2 f x1 x2 = set2With dfsStrategy f x1 x2

set2With :: Strategy b -> (a1 -> a2 -> b) -> a1 -> a2 -> Values b
set2With s f x1 x2 = allVs s (f (cover x1) (cover x2))

--- Combinator to transform a function of arity 3
--- into a corresponding set function.
set3 :: (a1 -> a2 -> a3 -> b) -> a1 -> a2 -> a3 -> Values b
set3 f x1 x2 x3 = set3With dfsStrategy f x1 x2 x3

set3With :: Strategy b -> (a1 -> a2 -> a3 -> b) -> a1 -> a2 -> a3 -> Values b
set3With s f x1 x2 x3 = allVs s (f (cover x1) (cover x2) (cover x3))

--- Combinator to transform a function of arity 4
--- into a corresponding set function.
set4 :: (a1 -> a2 -> a3 -> a4 -> b) -> a1 -> a2 -> a3 -> a4 -> Values b
set4 f x1 x2 x3 x4 = set4With dfsStrategy f x1 x2 x3 x4

set4With :: Strategy b -> (a1 -> a2 -> a3 -> a4 -> b) -> a1 -> a2 -> a3 -> a4 -> Values b
set4With s f x1 x2 x3 x4 = allVs s (f (cover x1) (cover x2) (cover x3) (cover x4))

--- Combinator to transform a function of arity 5
--- into a corresponding set function.
set5 :: (a1 -> a2 -> a3 -> a4 -> a5 -> b)
      -> a1 -> a2 -> a3 -> a4 -> a5 -> Values b
set5 f x1 x2 x3 x4 x5 = set5With dfsStrategy f x1 x2 x3 x4 x5

set5With :: Strategy b -> (a1 -> a2 -> a3 -> a4 -> a5 -> b)
      -> a1 -> a2 -> a3 -> a4 -> a5 -> Values b
set5With s f x1 x2 x3 x4 x5 = allVs s (f (cover x1)(cover x2)(cover x3)(cover x4)(cover x5))

--- Combinator to transform a function of arity 6
--- into a corresponding set function.
set6 :: (a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> b)
      -> a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> Values b
set6 f x1 x2 x3 x4 x5 x6 = set6With dfsStrategy f x1 x2 x3 x4 x5 x6

set6With :: Strategy b -> (a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> b)
      -> a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> Values b
set6With s f x1 x2 x3 x4 x5 x6 =
 allVs s (f (cover x1)(cover x2)(cover x3)(cover x4)(cover x5)(cover x6))

--- Combinator to transform a function of arity 7
--- into a corresponding set function.
set7 :: (a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> a7 -> b)
      -> a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> a7 -> Values b
set7 f x1 x2 x3 x4 x5 x6 x7 = set7With dfsStrategy f x1 x2 x3 x4 x5 x6 x7

set7With :: Strategy b -> (a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> a7 -> b)
      -> a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> a7 -> Values b
set7With s f x1 x2 x3 x4 x5 x6 x7 = 
 allVs s (f (cover x1)(cover x2)(cover x3)
            (cover x4)(cover x5)(cover x6)(cover x7))

------------------------------------------------------------------------
-- Axuiliaries:

-- collect all values of an expression in a list:
allVs s x = Values (vsToList (s (someSearchTree x)))

-- Function that covers identifiers
cover :: a -> a
cover external


------------------------------------------------------------------------
--- Abstract type representing multisets of values.

data Values a = Values [a]

--- Is a multiset of values empty?
isEmpty :: Values _ -> Bool
isEmpty (Values vs) = null vs

--- Is some value an element of a multiset of values?
valueOf :: a -> Values a -> Bool
valueOf e (Values s) = e `elem` s

--- Chooses (non-deterministically) some value in a multiset of values
--- and returns the chosen value and the remaining multiset of values.
--- Thus, if we consider the operation `chooseValue` by
---
---     chooseValue x = fst (choose x)
---
--- then `(set1 chooseValue)` is the identity on value sets, i.e.,
--- `(set1 chooseValue s)` contains the same elements as the
--- value set `s`.
choose :: Values a -> (a,Values a)
choose (Values vs) = (x, Values xs)
  where x = foldr1 (?) vs
        xs = delete x vs

--- Chooses (non-deterministically) some value in a multiset of values
--- and returns the chosen value.
--- Thus, `(set1 chooseValue)` is the identity on value sets, i.e.,
--- `(set1 chooseValue s)` contains the same elements as the
--- value set `s`.
chooseValue :: Values a -> a
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
select (Values (x:xs)) = (x, Values xs)

--- Selects (indeterministically) some value in a multiset of values
--- and returns the selected value.
--- Thus, `selectValue` has always at most one value.
--- It fails if the value set is empty.
---
--- **NOTE:**
--- The usage of this operation is only safe (i.e., does not destroy
--- completeness) if all values in the argument set are identical.
selectValue :: Values a -> a
selectValue s = fst (select s)

--- Chooses (non-deterministically) some value in a multiset of values
--- and returns the chosen value and the remaining multiset of values.
--- Thus, if we consider the operation `chooseValue` by
---
---     chooseValue x = fst (choose x)
---
--- then `(set1 chooseValue)` is the identity on value sets, i.e.,
--- `(set1 chooseValue s)` contains the same elements as the
--- value set `s`.
choose :: Values a -> (a,Values a)
choose (Values _ vs) = (x, Values (null xs) xs)
  where x = foldr1 (?) vs
        xs = delete x vs

--- Chooses (non-deterministically) some value in a multiset of values
--- and returns the chosen value.
--- Thus, `(set1 chooseValue)` is the identity on value sets, i.e.,
--- `(set1 chooseValue s)` contains the same elements as the
--- value set `s`.
chooseValue :: Values a -> a
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
select (Values _ (x:xs)) = (x, Values (null xs) xs)

--- Selects (indeterministically) some value in a multiset of values
--- and returns the selected value.
--- Thus, `selectValue` has always at most one value.
--- It fails if the value set is empty.
---
--- **NOTE:**
--- The usage of this operation is only safe (i.e., does not destroy
--- completeness) if all values in the argument set are identical.
selectValue :: Values a -> a
selectValue s = fst (select s)

--- Accumulates all elements of a multiset of values by applying a binary
--- operation. This is similarly to fold on lists, but the binary operation
--- must be <b>commutative</b> so that the result is independent of the order
--- of applying this operation to all elements in the multiset.
mapValues :: (a -> b) -> Values a -> Values b
mapValues f (Values s) = Values (map f s)

--- Accumulates all elements of a multiset of values by applying a binary
--- operation. This is similarly to fold on lists, but the binary operation
--- must be <b>commutative</b> so that the result is independent of the order
--- of applying this operation to all elements in the multiset.
foldValues :: (a -> a -> a) -> a -> Values a -> a
foldValues f z (Values s) = foldr f z s

--- Returns the minimal element of a non-empty multiset of values
--- with respect to a given total ordering on the elements.
minValue :: (a -> a -> Bool) -> Values a -> a
minValue leq (Values s) = minOf s
 where
  minOf [x] = x
  minOf (x:y:ys) = let m1 = minOf (y:ys)
                    in if leq x m1 then x else m1

--- Returns the maximal element of a non-empty multiset of value
--- with respect to a given total ordering on the elements.
maxValue :: (a -> a -> Bool) -> Values a -> a
maxValue leq (Values s) = maxOf s
 where
  maxOf [x] = x
  maxOf (x:y:ys) = let m1 = maxOf (y:ys)
                    in if leq x m1 then m1 else x

--- Puts all elements of a multiset of values in a list.
--- Since the order of the elements in the list might depend on
--- the time of the computation, this operation is an I/O action.
values2list :: Values a -> IO [a]
values2list (Values s) = return s

--- Prints all elements of a multiset of values.
printValues :: Values _ -> IO ()
printValues s = values2list s >>= mapIO_ print

--- Transforms a multiset of values into a list sorted by
--- the standard term ordering. As a consequence, the multiset of values
--- is completely evaluated.
sortValues :: Values a -> [a]
sortValues = sortValuesBy (<=)

--- Transforms a multiset of values into a list sorted by a given ordering
--- on the values. As a consequence, the multiset of values
--- is completely evaluated.
--- In order to ensure that the result of this operation is independent of the
--- evaluation order, the given ordering must be a total order.
sortValuesBy :: (a -> a -> Bool) -> Values a -> [a]
sortValuesBy leq (Values s) = mergeSort leq s

------------------------------------------------------------------------
