------------------------------------------------------------------------------
--- This library defines a data structure for sequence of values.
--- It is used in search trees (module `SearchTree`) as well as in
--- set functions (module `SetFunctions`).
--- Using sequence of values (rather than standard lists of values)
--- is necessary to get the behavior of set functions
--- w.r.t. finite failures right, as described in the paper
---
--- > J. Christiansen, M. Hanus, F. Reck, D. Seidel:
--- > A Semantics for Weakly Encapsulated Search in Functional Logic Programs
--- > Proc. 15th International Conference on Principles and Practice
--- > of Declarative Programming (PPDP'13), pp. 49-60, ACM Press, 2013
---
--- Note that the implementation for PAKCS is simplified in order to provide
--- some functionality used by other modules.
--- In particular, the intended semantics of failures is not provided
--- in the PAKCS implementation.
---
--- @author  Fabian Reck
--- @version November 2016
--- @category algorithm
------------------------------------------------------------------------------
{-# LANGUAGE CPP #-}

module Algorithm.ValueSequence
  (ValueSequence, emptyVS, addVS, failVS, (|++|), vsToList)
  where

--- A value sequence is an abstract sequence of values.
--- It also contains failure elements in order to implement the semantics
--- of set functions w.r.t. failures in the intended manner (only in KiCS2).
#ifdef __PAKCS__
data ValueSequence a = EmptyVS | ConsVS a (ValueSequence a)
#else
external data ValueSequence _ -- external
#endif

--- An empty sequence of values.
emptyVS :: ValueSequence a
#ifdef __PAKCS__
emptyVS = EmptyVS
#else
emptyVS external
#endif

--- Adds a value to a sequence of values.
addVS :: a -> ValueSequence a -> ValueSequence a
#ifdef __PAKCS__
addVS = ConsVS
#else
addVS external
#endif

--- Adds a failure to a sequence of values.
--- The argument is the encapsulation level of the failure.
failVS :: Int -> ValueSequence a
#ifdef __PAKCS__
failVS _ = EmptyVS -- cannot be implemented in PAKCS!"
#else
failVS external
#endif

--- Concatenates two sequences of values.
(|++|) :: ValueSequence a -> ValueSequence a -> ValueSequence a
#ifdef __PAKCS__
xs |++| ys = case xs of EmptyVS     -> ys
                        ConsVS z zs -> ConsVS z (zs |++| ys)
#else
(|++|) external
#endif

--- Transforms a sequence of values into a list of values.
vsToList :: ValueSequence a -> [a]
#ifdef __PAKCS__
vsToList EmptyVS       = []
vsToList (ConsVS x xs) = x : vsToList xs
#else
vsToList external
#endif
