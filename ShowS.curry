--------------------------------------------------------------------------------
--- This library provides a type and combinators for show functions using
--- functional lists.
---
--- @author  Björn Peemöller
--- @version March 2015
--- @category general
--------------------------------------------------------------------------------
module ShowS where

type ShowS = String -> String

--- Prepend a string
showString :: String -> ShowS
showString s = (s ++)

--- Prepend a single character
showChar :: Char -> ShowS
showChar c = (c:)

--- Surround the inner show function with parentheses if the first argument
--- evaluates to `True`.
showParen  :: Bool -> ShowS -> ShowS
showParen True  s = showChar '(' . s . showChar ')'
showParen False s = s

--- Convert a value to `ShowS` using the standard show function.
shows :: a -> ShowS
shows = showString . show

--- Prepend a space
space :: ShowS
space = showChar ' '

--- Prepend a newline
nl :: ShowS
nl = showChar '\n'

--- Separate a list of `ShowS`
sep :: ShowS -> [ShowS] -> ShowS
sep _ []       = id
sep s xs@(_:_) = foldr1 (\ f g -> f . s . g) xs

--- Replicate a `ShowS` a given number of times
replicateS :: Int -> ShowS -> ShowS
replicateS n funcS
  | n <= 0    = id
  | otherwise = funcS . replicateS (n - 1) funcS

--- Concatenate a list of `ShowS`
concatS :: [ShowS] -> ShowS
concatS []       = id
concatS xs@(_:_) = foldr1 (\ f g -> f . g) xs
