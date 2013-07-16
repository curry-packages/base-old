------------------------------------------------------------------------------
--- This library defines a datastructure for a sequence of values
--- this is used to get the behavior of set functions
--- w.r.t finite failures right
---
--- @author  Fabian Reck
--- @version July 2013
------------------------------------------------------------------------------

-- A value sequence is a sequence of values that
-- implements the semantics of set functions w.r.t. failures
data ValueSequence _ -- external

emptyVS :: ValueSequence a
emptyVS external

addVS :: a -> ValueSequence a -> ValueSequence a
addVS external

failVS :: Int -> ValueSequence a
failVS external

vsToList :: ValueSequence a -> [a]
vsToList external

(|++|) :: ValueSequence a -> ValueSequence a -> ValueSequence a
(|++|) external