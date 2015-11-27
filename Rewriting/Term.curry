------------------------------------------------------------------------------
--- Library for representation of first-order terms.
---
--- This library is the basis of other libraries for the manipulation
--- of first-order terms, e.g., unification of terms.
--- Therefore, this library also defines other structures,
--- like term equations or positions.
---
--- @author  Michael Hanus, Jonas Oberschweiber, Bjoern Peemoeller
--- @version November 2015
--- @category algorithm
------------------------------------------------------------------------------

module Rewriting.Term
  ( VarIdx, Term (..), TermEq, TermEqs, Pos
  ) where

-- ---------------------------------------------------------------------------
-- Representation of terms
-- ---------------------------------------------------------------------------

--- Variable index, identifying a variable.
type VarIdx = Int

--- Representation of a first-order terms. It is parameterized over
--- the kind of function symbols, e.g., strings.
---
--- @cons TermVar i          - The variable with index `i`
--- @cons TermCons name args - The constructor with constructor `name`
---                            and argument terms `args`
data Term f = TermVar VarIdx | TermCons f [Term f]

--- The type of an equation.
type TermEq f = (Term f, Term f)

--- The type of multiple equations.
type TermEqs f = [TermEq f]

-- ---------------------------------------------------------------------------
-- Positions in a term.
-- ---------------------------------------------------------------------------

--- A position in a term represented as a list of integers.
--- Arguments are enumerated from 0.
type Pos = [Int]

