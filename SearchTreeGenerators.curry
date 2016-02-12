------------------------------------------------------------------------------
--- This library implements some operations to generate search trees
--- for various types.
---
--- @author  Michael Hanus
--- @version February 2016
--- @category algorithm
------------------------------------------------------------------------------

module SearchTreeGenerators
  ( genBool, genInt, genChar, genList
  ) where
 
import SearchTree
import SearchTreeTraversal

mapST :: (a -> b) -> SearchTree a -> SearchTree b
mapST f (Value a)  = Value (f a)
mapST _ (Fail i)   = Fail i
mapST f (Or t1 t2) = Or (mapST f t1) (mapST f t2)

mapSTVal :: (a -> SearchTree b) -> SearchTree a -> SearchTree b
mapSTVal f (Value a)  = (f a)
mapSTVal _ (Fail i)   = Fail i
mapSTVal f (Or t1 t2) = Or (mapSTVal f t1) (mapSTVal f t2)

data Nat    = IHi | O Nat | I Nat

nat2int :: Nat -> Int
nat2int IHi = 1
nat2int (O n) = 2 * nat2int n
nat2int (I n) = 2 * nat2int n + 1

data BinInt = Neg Nat | Zero | Pos Nat

b2int :: BinInt -> Int
b2int (Neg n) = 0 - nat2int n
b2int Zero = 0
b2int (Pos n) = nat2int n

genNat :: SearchTree Int
genNat = gen id
 where
  gen f = Or (Value (nat2int (f IHi))) (Or (gen (O . f)) (gen (I . f)))

--- Generators a search tree for integer values.
genInt :: SearchTree Int
genInt = Or (mapST (\n -> 0 - n) genNat)
            (Or (Value 0) genNat)
             
--- Generators a search tree for Boolean values.
genBool :: SearchTree Bool
genBool = Or (Value False) (Value True)

--- Generators a search tree for character values.
genChar :: SearchTree Char
genChar = mapST (\n -> chr (n+1)) genNat

--- Generators a search tree for list values where the search tree for
--- the elements is given as a parameter.
genList :: SearchTree a -> SearchTree [a]
genList genx = genL id
 where
  genL f = Or (Value (f []))
              (mapSTVal (\v -> genL ((v:) . f)) genx)
