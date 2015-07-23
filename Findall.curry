------------------------------------------------------------------------------
--- Library with some operations for encapsulating search.
--- Note that some of these operations are not fully declarative,
--- i.e., the results depend on the order of evaluation and program rules.
--- There are newer and better approaches the encpasulate search,
--- in particular, set functions (see module `SetFunctions`),
--- which should be used.
---
--- This library is included only for compatibility with PAKCS.
---
--- @author Michael Hanus
--- @version July 2015
--- @category general
------------------------------------------------------------------------------

module Findall
  ( getAllValues, getSomeValue
  , allValues, someValue
  , allSolutions, someSolution
  ) where

import qualified SearchTree as ST

--- Gets all values of an expression (currently, via an incomplete
--- depth-first strategy). Conceptually, all values are computed
--- on a copy of the expression, i.e., the evaluation of the expression
--- does not share any results. Moreover, the evaluation suspends
--- as long as the expression contains unbound variables.
--- Similar to Prolog's findall.
getAllValues :: a -> IO [a]
getAllValues e = return (allValues e)

--- Gets a value of an expression (currently, via an incomplete
--- depth-first strategy). The expression must have a value, otherwise
--- the computation fails. Conceptually, the value is computed on a copy
--- of the expression, i.e., the evaluation of the expression does not share
--- any results. Moreover, the evaluation suspends as long as the expression
--- contains unbound variables.
getSomeValue :: a -> IO a
getSomeValue e = return (someValue e)

--- Returns all values of an expression (currently, via an incomplete
--- depth-first strategy). Conceptually, all values are computed on a copy
--- of the expression, i.e., the evaluation of the expression does not share
--- any results.
---
--- Note that this operation is not purely declarative since the ordering
--- of the computed values depends on the ordering of the program rules.
allValues :: a -> [a]
allValues e = ST.allValuesDFS (ST.someSearchTree e)

--- Returns some value for an expression (currently, via an incomplete
--- depth-first strategy). If the expression has no value, the
--- computation fails.
--- Conceptually, the value is computed on a copy of the expression,
--- i.e., the evaluation of the expression does not share any results.
---
--- Note that this operation is not purely declarative since
--- the computed value depends on the ordering of the program rules.
--- Thus, this operation should be used only if the expression
--- has a single value.
someValue :: a -> a
someValue = ST.someValueWith ST.dfsStrategy

--- Returns all values satisfying a predicate, i.e., all arguments such that
--- the predicate applied to the argument can be evaluated to `True`
--- (currently, via an incomplete depth-first strategy).
---
--- Note that this operation is not purely declarative since the ordering
--- of the computed values depends on the ordering of the program rules.
allSolutions :: (a->Bool) -> [a]
allSolutions p = allValues (let x free in p x &&> x)

--- Returns some values satisfying a predicate, i.e., some argument such that
--- the predicate applied to the argument can be evaluated to `True`
--- (currently, via an incomplete depth-first strategy).
--- If there is no value satisfying the predicate, the computation fails.
---
--- Note that this operation is not purely declarative since the ordering
--- of the computed values depends on the ordering of the program rules.
--- Thus, this operation should be used only if the
--- predicate has a single solution.
someSolution :: (a->Bool) -> a
someSolution p = someValue (let x free in p x &&> x)

------------------------------------------------------------------------------
