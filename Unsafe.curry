------------------------------------------------------------------------------
--- Library containing unsafe operations.
--- These operations should be carefully used (e.g., for testing or debugging).
--- These operations should not be used in application programs!
---
--- @author Michael Hanus, Björn Peemöller
--- @version September 2013
------------------------------------------------------------------------------

{-# OPTIONS_CYMAKE -X TypeClassExtensions #-}

module Unsafe
  ( unsafePerformIO, trace
  ) where

import Char (isSpace)
import IO   (hPutStrLn, stderr)

--- Performs and hides an I/O action in a computation (use with care!).
unsafePerformIO :: IO a -> a
unsafePerformIO external

--- Prints the first argument as a side effect and behaves as identity on the
--- second argument.
trace :: String -> a -> a
trace s x = unsafePerformIO (hPutStrLn stderr s >> return x)
