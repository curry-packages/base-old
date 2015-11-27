------------------------------------------------------------------------------
--- Library for unification on first-order terms.
---
--- This library implements a unification algorithm using reference tables.
---
--- @author  Michael Hanus, Jonas Oberschweiber, Bjoern Peemoeller
--- @version November 2015
--- @category algorithm
------------------------------------------------------------------------------

module Rewriting.Unification ( UnificationError (..), unify ) where

import FiniteMap
import List (mapAccumL)
import Rewriting.Term
import Rewriting.Substitution
import Rewriting.UnificationSpec ( UnificationError(..) )

--- An RTerm is the unification algorithm's internal term representation.
--- Its RTermCons and RTermVar constructors are similar to the TermCons
--- and TermVar constructors of the original Term data type, but it has
--- an additional Ref constructor. This Ref constructor is used to
--- represent references into a RefTable.
data RTerm f
  = RTermCons f [RTerm f]
  | RTermVar  VarIdx
  | Ref       VarIdx

--- Type of the reference table used to store the values referenced
--- by Ref RTerms.
type RefTable f = FM VarIdx (RTerm f)

--- An equation of RTerms.
type REq f = (RTerm f, RTerm f)

--- A list of equations of RTerms.
type REqs f = [REq f]

--- Unifies the given equations.
---
--- @param eqs - the equations to unify
--- @return either an UnificationError or a substitution
unify :: TermEqs f -> Either (UnificationError f) (Subst f)
unify ts = let (r, rts) = termEqsToREqs ts in case unify' r [] rts of
  Right (r', ts') -> Right (eqsToSubst r' ts')
  Left err        -> Left err

-- ---------------------------------------------------------------------------
-- conversion to internal structure
-- ---------------------------------------------------------------------------

--- Converts a list of equations of terms into a list of equations
--- of RTerms. Places references into a fresh RefTable.
---
--- @param eqs - the equations to convert
--- @return a tuple of the newly created RefTable and the converted
--- equations
termEqsToREqs :: TermEqs f -> (RefTable f, REqs f)
termEqsToREqs l = mapAccumL termEqToREq (emptyFM (<)) l

--- Converts an equation of terms into an equation of RTerms.
---
--- @param tab - the RefTable to use for storing references
--- @param eq - the equation to convert
--- @return a tuple of the RefTable and the converted equation
termEqToREq :: RefTable f -> TermEq f -> (RefTable f, REq f)
termEqToREq r (a, b) = let (r1, a') = termToRTerm r  a
                           (r2, b') = termToRTerm r1 b
                       in  (r2, (a', b'))

--- Converts a Term to an RTerm, placing all TermVars in the given RefTable
--- and replacing them by references inside the result RTerm.
---
--- @param tab - the RefTable to place references in
--- @param term - the Term to convert to RTerm
--- @return a tuple of the RefTable and the converted RTerm
termToRTerm :: RefTable f -> Term f -> (RefTable f, RTerm f)
termToRTerm r (TermVar    i) = (addToFM r i (RTermVar i), Ref i)
termToRTerm r (TermCons n l) = let (r', l') = mapAccumL termToRTerm r l
                               in  (r', RTermCons n l')

-- ---------------------------------------------------------------------------
-- conversion from internal structure
-- ---------------------------------------------------------------------------

--- Converts a list of term equations to a substitution
--- by turning every equation of the form (TermVar n, a) or (a, TermVar n)
--- into a mapping (n, a).
--- Equations that do not have a TermVar on either side are ignored.
--- Works on RTerms, dereferences all Refs.
---
--- @param eqs - the equations to convert
--- @return the resulting substitution
eqsToSubst :: RefTable f -> REqs f -> Subst f
eqsToSubst _ []            = emptySubst
eqsToSubst r ((a, b) : ts) = case a of
  Ref _         -> eqsToSubst r ((deref r a, b) : ts)
  RTermVar n    -> extendSubst (eqsToSubst r ts) n (rTermToTerm r b)
  RTermCons _ _ -> case b of
    RTermVar n  -> extendSubst (eqsToSubst r ts) n (rTermToTerm r a)
    Ref _       -> eqsToSubst r ((a, deref r b) : ts)
    _           -> eqsToSubst r ts

--- Converts an RTerm to a Term, dereferencing all references inside
--- the RTerm.
---
--- @param tab - the RefTable to use for reference lookups
--- @param term - the RTerm to convert to Term
--- @return the converted Term
rTermToTerm :: RefTable f -> RTerm f -> Term f
rTermToTerm r i@(Ref       _) = rTermToTerm r (deref r i)
rTermToTerm _ (RTermVar    n) = TermVar n
rTermToTerm r (RTermCons n l) = TermCons n (map (rTermToTerm r) l)

--- Dereferences an RTerm, following chained references.
--- Simply returns the same value for RTermVar and RTermCons.
---
--- @param tab - the RefTable to use for reference lookups
--- @param t - the RTerm to dereference
--- @return the dereferenced RTerm
deref :: RefTable f -> RTerm f -> RTerm f
deref t (Ref           i) = case lookupFM t i of
  Nothing -> error $ "Unification.deref: " ++ show i
  Just a  -> case a of
    RTermVar _    -> a
    RTermCons _ _ -> a
    Ref _         -> deref t a
deref _ a@(RTermVar    _) = a
deref _ a@(RTermCons _ _) = a

-- ---------------------------------------------------------------------------
-- unification algorithm
-- ---------------------------------------------------------------------------

--- Internal unification function, the core of the algorithm.
unify' :: RefTable f -> REqs f -> REqs f
       -> Either (UnificationError f) (RefTable f, REqs f)
-- No equations left, we are done.
unify' r s []             = Right (r, s)
unify' r s ((a, b) : eqs) = case (a, b) of
  -- Substitute the constructor for the variable.
  (RTermVar    i, RTermCons _ _) -> elim r s i b eqs
  (RTermCons _ _, RTermVar    i) -> elim r s i a eqs
  -- If both vars are equal, simply remove the equation.
  -- Otherwise substitute the second var for the first var.
  (RTermVar    i, RTermVar   i') | i == i'   -> unify' r s eqs
                                 | otherwise -> elim   r s i b eqs
  -- If both constructors have the same name, equate their arguments.
  -- Otherwise fail with a clash.
  (RTermCons ca as, RTermCons cb bs)
    | ca == cb  -> unify' r s (zip as bs ++ eqs)
    | otherwise -> Left $ Clash (rTermToTerm r a) (rTermToTerm r b)
  -- If we encounter a Ref, simply dereference it and try again.
  _  -> unify' r s ((deref r a, deref r b) : eqs)

--- Substitutes a term for a variable inside the list of equations
--- that have yet to be unified and the right-hand sides of all
--- equations of the result list. Also adds a mapping from that
--- variable to that term to the result list.
elim :: RefTable f -> REqs f -> VarIdx -> RTerm f -> REqs f
     -> Either (UnificationError f) (RefTable f, REqs f)
elim r s i t eqs
  | dependsOn r (RTermVar i) t = Left (OccurCheck i (rTermToTerm r t))
  | otherwise                  = case t of
    -- Make sure to place a Ref in the RefTable and Subst,
    -- not the RTermVar itself.
    RTermVar i'   -> unify' (addToFM r i (Ref i')) ((RTermVar i, Ref i'):s) eqs
    RTermCons _ _ -> unify' (addToFM r i t       ) ((RTermVar i,      t):s) eqs
    _             -> error "Unification.elim"

--- Checks wether the first term occurs as a subterm of the second term.
--- @param a - the term to search for
--- @param b - the term to search
--- @return whether the first term is found in the second term
dependsOn :: RefTable f -> RTerm f -> RTerm f -> Bool
dependsOn r a b = a /= b && dependsOn' b
 where
  dependsOn' v@(RTermVar   _) = a == v
  dependsOn' (RTermCons _ vs) = or (map dependsOn' vs)
  dependsOn' x@(Ref        _) = deref r x == a
