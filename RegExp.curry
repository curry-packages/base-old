------------------------------------------------------------------------------
--- A library to defined and match regular expressions.
--- This library is used to translated integrated code in the form
--- of POSIX extended regular expressions into Curry programs.
---
--- @author Jasper Sikorra
--- @version January 2013
--- @category general
------------------------------------------------------------------------------
-- TODO
-- - Stard and End are not implemented
-- - No function to match only a part of a lists

module RegExp(match, RegExp, ORegExp(..)) where

import List

--- Data type for regex representation in Curry
type RegExp a = [ORegExp a]
data ORegExp a = Nil
               | Literal a
               | Xor (RegExp a) (RegExp a)
               | Star (RegExp a)
               | Plus (RegExp a)
               | AnyLiteral
               | Bracket [Either a (a,a)]
               | NegBracket [Either a (a,a)]
               | Start (RegExp a)
               | End (RegExp a)
               | Times (Int,Int) (RegExp a)

--- The match function is used to match lists with regular expressions
--- @param s - The input list
--- @param r - The regular expression
--- @result True if matched else False
match :: Ord a => [a] -> RegExp a -> Bool
match s re = case re of
  []                  -> s == []
  (Nil:ors)           -> match s ors
  (Xor or1 or2:ors)   -> match s (or1 ++ ors) || match s (or2 ++ ors)
  (Literal c:ors)     -> case s of
    []      -> False
    (d:ds)  -> if (d == c) then match ds ors else False
  (Star r:ors)        -> matchstar s r ors
  (Plus r:ors)        -> matchplus s r ors
  (AnyLiteral:ors)    -> case s of
    []      -> False
    (_:ds)  -> match ds ors
  (Bracket b:ors)     -> case s of
    []      -> False
    (d:ds)  -> (matchbracket b d) && match ds ors
  (NegBracket b:ors)  -> case s of
    []      -> False
    (d:ds)  -> not (matchbracket b d) && match ds ors
  (Start _:_)         -> failed
  (End _:_)           -> failed
  (Times (n,m) r:ors) -> matchtimes s n m r ors

-- Matching with a star
matchstar :: Ord a => [a] -> RegExp a -> RegExp a -> Bool
matchstar st r rgx = (||)
  (match st rgx)
  (tryeach (map (\x -> match x r) (inits st)) (tails st) r rgx)

tryeach :: Ord a => [Bool] -> [[a]] -> RegExp a -> RegExp a -> Bool
tryeach [] []         _  _   = False
tryeach (b:bs) (t:ts) r  rgx = 
  (||)
    (if b
      then
        (match t rgx || matchstar t r rgx)
      else False)
    (tryeach bs ts r rgx)

-- Matching with a plus
matchplus :: Ord a => [a] -> RegExp a -> RegExp a -> Bool
matchplus st r rgx = tryeach (map (\x -> match x r) ini) tls r rgx
  where
    ini = tail (inits st)
    tls = tail (tails st)

-- Matching with a bracket
matchbracket :: Ord a => [Either a (a,a)] -> a -> Bool
matchbracket [] _                          = False
matchbracket (Left c:es)        d | c == d = True
                                  | c /= d = matchbracket es d
matchbracket (Right (c1,c2):es) d          =
  (||)
    (d >= c1 && d <= c2)
    (matchbracket es d)

-- Matching an amount of times between a range
matchtimes :: Ord a => [a] -> Int -> Int -> RegExp a -> RegExp a -> Bool
matchtimes s n m r rgx | m == 0 = match s rgx
                       | m >  0 =
  tryeachRestricted (m-n) (map (\x -> match x mr) (inits s)) (tails s) r rgx
  where
    mr = concat (replicate n r)

tryeachRestricted :: Ord a => Int -> [Bool] -> [[a]] -> RegExp a -> RegExp a
                  -> Bool
tryeachRestricted _      []     []     _   _   = False
tryeachRestricted m      (b:bs) (t:ts) r  rgx  =
  (||)
    (if b
      then
        (match t rgx || matchtimes t 1 m r rgx)  
      else False)
    (tryeachRestricted m bs ts r rgx)

------------------------------------------------------------------------------
