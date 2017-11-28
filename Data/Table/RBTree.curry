---------------------------------------------------------------------------
--- Library with an implementation of tables as red-black trees:
--- <P>
--- A table is a finite mapping from keys to values.
--- All the operations on tables are generic, i.e., one has to provide
--- an explicit order predicate ("<CODE>cmp</CODE>" below) on elements.
--- Each inner node in the red-black tree contains a key-value association.
---
--- @author Johannes Koj, Michael Hanus, Bernd Brassel
--- @version March 2005
--- @category algorithm
----------------------------------------------------------------------------

module Data.Table.RBTree where

import qualified Data.RedBlackTree as RBT
--import RedBlackTree (RedBlackTree) -- uncomment for old (buggy) Java front end

----------------------------------------------------------------------------
-- the main interface:

type TableRBT key a = RBT.RedBlackTree (key,a)

--- Returns an empty table, i.e., an empty red-black tree.
empty :: Eq key => TableRBT key a
empty = RBT.empty (\ x y -> fst x == fst y)
                  (\ x y -> fst x == fst y)

--- tests whether a given table is empty
isEmpty :: TableRBT _ _ -> Bool
isEmpty = RBT.isEmpty

--- Looks up an entry in a table.
--- @param k - a key under which a value is stored
--- @param t - a table (represented as a red-black tree)
--- @return (Just v) if v is the value stored with key k,
---         otherwise Nothing is returned.
lookup :: (Ord key, Ord a) => key -> TableRBT key a -> Maybe a
lookup k = maybe Nothing (Just . snd) . RBT.lookup (k,failed)

--- Inserts or updates an element in a table.
update :: (Ord key, Ord a) => key -> a -> TableRBT key a -> TableRBT key a
update k e = RBT.update (k,e)

--- Transforms the nodes of red-black tree into a list.
toList :: TableRBT key a -> [(key,a)]
toList = RBT.toList

delete :: (Ord key, Ord a) => key -> TableRBT key a -> TableRBT key a
delete key = RBT.delete (key,failed)

-- end of TableRBT
