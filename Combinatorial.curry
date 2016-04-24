------------------------------------------------------------------------------
--- A collection of common non-deterministic and/or combinatorial operations.
--- Many operations are intended to operate on sets.
--- The representation of these sets is not hidden; rather
--- sets are represented as lists.
--- Ideally these lists contains no duplicate elements and
--- the order of their elements cannot be observed.
--- In practice, these conditions are not enforced.
---
--- @author Sergio Antoy (with extensions by Michael Hanus)
--- @version April 2016
--- @category general
------------------------------------------------------------------------------

module Combinatorial(permute, subset, allSubsets, splitSet,
                     sizedSubset, partition) where

import List(sum)
import SetFunctions
import Test.Prop

------------------------------------------------------------------------------
--                       Public Operations
------------------------------------------------------------------------------

--- Compute any permutation of a list.
---
--- @param xs - The list.
--- @return A permutation of the argument.

permute        :: [a] -> [a]
permute []     = []
permute (x:xs) = ndinsert (permute xs)
    where ndinsert [] = [x]
          ndinsert (y:ys) = (x:y:ys) ? (y:ndinsert ys)

-- Properties:
permute1234 = permute [1,2,3,4] ~> [1,3,4,2]
-- The length of a permutation is identical to the length of the argument:
permLength xs = length (permute xs) <~> length xs -- lengths are equal
-- The permutation contains the same elements as the argument:
permElems xs = anyOf (permute xs) <~> anyOf xs


------------------------------------------------------------------------------
--- Compute any sublist of a list.
--- The sublist contains some of the elements of the list in the same order.
---
--- @param xs - The list.
--- @return A sublist of the argument.

subset        :: [a] -> [a]
subset []     = []
subset (x:xs) = x:subset xs
subset (_:xs) =   subset xs

-- Properties:
subset1234 = subset [1,2,3,4] ~> [1,3]
subset123  = subset [1,2,3] <~> ([1,2,3]?[1,2]?[1,3]?[1]?[2,3]?[2]?[3]?[])
subsetElems xs = anyOf (subset xs) <~ anyOf xs

------------------------------------------------------------------------------
--- Compute all the sublists of a list.
---
--- @param xs - The list.
--- @return All the sublists of the argument.

allSubsets      :: [a] -> [[a]]
allSubsets xs = sortValues (set1 subset xs)

-- Properties:
allSubsets123 =
  allSubsets [1,2,3] -=- [[],[1],[1,2],[1,2,3],[1,3],[2],[2,3],[3]]


------------------------------------------------------------------------------
--- Split a list into any two sublists.
---
--- @param xs - The list.
--- @return A pair consisting of two complementary sublists of the argument.

splitSet    :: [a] -> ([a],[a])
splitSet []     = ([],[])
splitSet (x:xs) = let (u,v) = splitSet xs in (x:u,v) ? (u,x:v)

-- Properties:
splitSet1234 = splitSet [1,2,3,4] ~> ([1,3,4],[2])
-- The sum of the length of the two sublists is the length of the argument list:
splitSetLengths xs =
  (\ (xs,ys) -> length xs + length ys) (splitSet xs) <~> length xs
-- The two sublists and the argument list have the same elements:
splitSetElems xs =
  (\ (xs,ys) -> anyOf xs ? anyOf ys) (splitSet xs) <~> anyOf xs


------------------------------------------------------------------------------
--- Compute any sublist of fixed length of a list.
--- Similar to 'subset', but the length of the result is fixed.
---
--- @param c - The length of the output sublist.
--- @param xs - The input list.
--- @return A sublist of `xs` of length `c`.

sizedSubset     :: Int -> [a] -> [a]
sizedSubset c l = if c == 0 then [] else aux l
    where aux (x:xs) = x:sizedSubset (c-1) xs ? sizedSubset c xs

-- Precondition:
sizedSubset'pre c _ = c>=0

-- Properties:
sizedSubsetLength c xs =
 (c>=0 && length xs >= c) ==> length (sizedSubset c xs) <~> c
-- No result if the given output length is larger than the length of the input:
sizedSubsetLengthTooSmall c xs =
 (c>=0 && length xs < c) ==> failing (sizedSubset c xs)


------------------------------------------------------------------------------
--- Compute any partition of a list.
--- The output is a list of non-empty lists such that their concatenation
--- is a permutation of the input list.
--- No guarantee is made on the order of the arguments in the output.
---
--- @param xs - The input list.
--- @return A partition of `xs` represented as a list of lists.

partition    :: [a] -> [[a]]
partition [] = []
partition (x:xs) = insert x (partition xs)
    where insert e [] = [[e]]
          insert e (y:ys) = ((e:y):ys) ? (y:insert e ys)

-- Properties:
partition1234 = partition [1,2,3,4] ~> [[4],[2,3],[1]]
partition123  =
  partition [1,2,3]
  <~>
  ([[1,2,3]] ? [[2,3],[1]] ? [[1,3],[2]] ? [[3],[1,2]] ? [[3],[2],[1]])
-- The sum of the length of the sublists is the length of the argument list:
partitionLengths xs = sum (map length (partition xs)) <~> length xs
-- The sublists of the partition and the argument list have the same elements:
partitionElems xs = anyOf (map anyOf (partition xs)) <~> anyOf xs

-- end module Combinatorial
