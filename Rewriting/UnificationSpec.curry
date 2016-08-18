------------------------------------------------------------------------------
--- Library for specifying the unification on first-order terms.
---
--- This library implements a general unification algorithm.
--- Because the algorithm is easy to understand, but rather slow,
--- it serves as a specification for more elaborate implementations.
---
--- @author  Michael Hanus, Jonas Oberschweiber, Bjoern Peemoeller
--- @version November 2015
--- @category algorithm
------------------------------------------------------------------------------

module Rewriting.UnificationSpec ( UnificationError (..), unify ) where

import FiniteMap
import Rewriting.Term
import Rewriting.Substitution

-- ---------------------------------------------------------------------------
-- unification
-- ---------------------------------------------------------------------------

--- The data type for the different kinds of errors that can occur during
--- unification.
---
--- @cons Clash t1 t2    - Two term constructors with different names
---                        are supposed to be equal.
--- @cons OccurCheck v t - A term is supposed to be equal to a term
---                        in which it occurs as a subterm.
data UnificationError f = Clash (Term f) (Term f)
                        | OccurCheck VarIdx (Term f)

--- Unifies the given equations.
---
--- @param eqs - the equations to unify
--- @return either a UnificationError or a substitution
unify :: (Eq f, Show f) => TermEqs f -> Either (UnificationError f) (Subst f)
unify ts = either Left (Right . eqsToSubst) (unify' [] ts)

eqsToSubst :: Show f => TermEqs f -> Subst f
eqsToSubst []            = emptySubst
eqsToSubst ((a, b) : ts) = case a of
  TermVar n      -> extendSubst (eqsToSubst ts) n b
  TermCons _ _   -> case b of
    TermVar n    -> extendSubst (eqsToSubst ts) n a
    _            -> error $ "eqsToSubst: " ++ show (a, b)

--- Auxiliary unification operation.
--- @param sub - the current substitution represented by term equations
--- @param eqs - the equations to unify
unify' :: Eq f => TermEqs f -> TermEqs f -> Either (UnificationError f) (TermEqs f)
unify' s [] = Right s
unify' s (((TermVar      i), b@(TermCons _ _)):e) = elim s i b e
unify' s ((a@(TermCons _ _), (TermVar      i)):e) = elim s i a e
unify' s ((TermVar        i, b@(TermVar   i')):e) | i == i'   = unify' s e
                                                  | otherwise = elim s i b e
unify' s ((a@(TermCons ac as), b@(TermCons bc bs)):e)
  | ac == bc  = unify' s ((zip as bs) ++ e)
  | otherwise = Left (Clash a b)

elim :: Eq f => TermEqs f -> VarIdx -> Term f -> TermEqs f
     -> Either (UnificationError f) (TermEqs f)
elim s i t e
  | dependsOn (TermVar i) t = Left (OccurCheck i t)
  | otherwise               = unify' s' (substitute' i t e)
    where s' = (TermVar i, t) : map (\(x, y) -> (x, termSubstitute' i t y)) s

--- Substitutes a variable with the given index by a term inside another term.
---
--- @param i - the index of the variable to substitute
--- @param t - the term to substitute
--- @param subj - the term to substitute in
--- @return the resulting term
termSubstitute' :: VarIdx -> Term f -> Term f -> Term f
termSubstitute' b s v@(TermVar n)
    | n == b    = s
    | otherwise = v
termSubstitute' b s (TermCons t vars) = TermCons t (termsSubstitute' b s vars)

--- Substitute a variable with the given index by a term inside a list of terms.
---
--- @param i - the index of the variable to substitute
--- @param t - the term to substitute
--- @param subjs - the terms to substitute in
--- @return the resulting terms
termsSubstitute' :: VarIdx -> Term f -> [Term f] -> [Term f]
termsSubstitute' i t ts = map (termSubstitute' i t) ts

--- Substitute a variable with the given index by a term inside a list of equations.
---
--- @param i - the index of the variable to substitute
--- @param t - the term to substitute
--- @param eqs - the equations to substitute in
--- @return the resulting equations
substitute' :: VarIdx -> Term f -> TermEqs f -> TermEqs f
substitute' i t ts = map (substituteSingle' i t) ts

--- Substitute a variable with the given index by a term inside a single equation.
---
--- @param i - the index of the variable to substitute
--- @param t - the term to substitute
--- @param eq - the equation to substitute in
--- @return the resulting equation
substituteSingle' :: VarIdx -> Term f -> TermEq f -> TermEq f
substituteSingle' i t (a, b) = (termSubstitute' i t a, termSubstitute' i t b)

--- Checks wether the first term occurs as a subterm of the second term.
--- @param a - the term to search for
--- @param b - the term to search in
--- @return whether the first term is found in the second term
dependsOn :: Eq f => Term f -> Term f -> Bool
dependsOn a b = and [(not (a == b)), dependsOn' a b]
 where dependsOn' c v@(TermVar _) = c == v
       dependsOn' c (TermCons _ vars) = any id (map (dependsOn' c) vars)
