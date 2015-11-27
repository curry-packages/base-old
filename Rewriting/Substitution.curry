------------------------------------------------------------------------------
--- Library for representation of substitutions on first-order terms.
---
--- @author  Michael Hanus, Jonas Oberschweiber, Bjoern Peemoeller
--- @version November 2015
--- @category algorithm
------------------------------------------------------------------------------

module Rewriting.Substitution
  ( Subst
  , showSubst, emptySubst, extendSubst, lookupSubst, applySubst
  ) where

import FiniteMap
import Rewriting.Term

-- ---------------------------------------------------------------------------
-- Substitution on terms
-- ---------------------------------------------------------------------------

--- The (abstract) data type for substitutions.
type Subst f = FM VarIdx (Term f)

--- Pretty string representation of a substitution.
showSubst :: Subst _ -> String
showSubst = unlines . map showOne . fmToList
  where showOne (k, v) = show k ++ " -> " ++ show v

--- The empty substitution
emptySubst :: Subst _
emptySubst = emptyFM (<)

--- Extend the substitution with the given mapping.
---
--- @param subst         - the substitution
--- @param index         - the variable which should be mapped
--- @param term          - the term the variable should be mapped to
--- @return                the extended substitution
extendSubst :: Subst f -> VarIdx -> Term f-> Subst f
extendSubst = addToFM

--- Searches the substitution for a mapping from the given variable index
--- to a term.
---
--- @param subst - the substitution to search
--- @param i - the index to search for
--- @return the found term or Nothing
lookupSubst :: Subst f -> VarIdx -> Maybe (Term f)
lookupSubst = lookupFM

--- Applies a substitution to a single term.
---
--- @param sub - the substitution to apply
--- @param t - the term to apply the substitution to
--- @return the resulting term
applySubst :: Subst f -> Term f -> Term f
applySubst s t@(TermVar   n) = maybe t id (lookupSubst s n)
applySubst s (TermCons c vs) = TermCons c (map (applySubst s) vs)

--- Applies a substitution to a single equation.
---
--- @param sub - the substitution to apply
--- @param eq - the equation to apply the substitution to
--- @return the resulting equation
substituteSingle :: Subst f -> TermEq f -> TermEq f
substituteSingle s (a, b) = (applySubst s a, applySubst s b)

--- Applies a substitution to a list of equations.
---
--- @param sub - the substitution to apply
--- @param eqs - the equations to apply the substitution to
--- @return the resulting equations
substitute :: Subst f -> TermEqs f -> TermEqs f
substitute s eqs = map (substituteSingle s) eqs

