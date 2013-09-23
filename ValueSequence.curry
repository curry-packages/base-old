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
--- @author  Fabian Reck
--- @version July 2013
------------------------------------------------------------------------------

module ValueSequence(ValueSequence, emptyVS, addVS, failVS, (|++|), vsToList)
 where

--- A value sequence is an abstract sequence of values.
--- It also contains failure elements in order to implement the semantics
--- of set functions w.r.t. failures in the intended manner.
data ValueSequence _ -- external

--- An empty sequence of values.
emptyVS :: ValueSequence a
emptyVS external

--- Adds a value to a sequence of values.
addVS :: a -> ValueSequence a -> ValueSequence a
addVS external

--- Adds a failure to a sequence of values.
--- The argument is the encapsulation level of the failure.
failVS :: Int -> ValueSequence a
failVS external

--- Concatenates two sequences of values.
(|++|) :: ValueSequence a -> ValueSequence a -> ValueSequence a
(|++|) external

--- Transforms a sequence of values into a list of values.
vsToList :: ValueSequence a -> [a]
vsToList external
