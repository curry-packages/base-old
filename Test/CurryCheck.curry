-------------------------------------------------------------------------
--- Module to support CurryCheck tool.
--- Will be propably further developed in the future.
-------------------------------------------------------------------------

module Test.CurryCheck
  ( CTest, PropIO
  , (==>), (-=-), (<~), (~>), (<~>), yields, sameAs
  , anyOf, anySolutionOf

  -- internal functions used by by the CurryCheck tool
  , execAssertIO, makeProp, execPropWithMsg, execProps
  ) where

import AllSolutions
import Distribution     (curryCompiler, installDir)
import qualified Test.EasyCheck
import IO               (hFlush, stdout)
import Maybe            (catMaybes)
import ReadShowTerm     (writeQTermFile)
import System           (system, exitWith)

infix  1 -=-, <~, ~>, <~>, `yields`, `sameAs`
infixr 0 ==>

--- Abstract data type to represent different kind of property tests.
data CTest a
  = EqualTest a a
  | CondTest Bool (CTest a)
  | SubSetTest a a
  | SetTest a a

-- restrict generated test data used
-- p must be true for t being executed
(==>) :: Bool -> CTest a -> CTest a
p ==> t = CondTest p t

-- compare the result sets of two expressions
-- (multi-set semantics)
(-=-) :: a -> a -> CTest a
(-=-) = EqualTest

-- compare the result sets of two expressions
-- (set semantics)

-- subset
(<~) :: a -> a -> CTest a
(<~) = SubSetTest

-- superset
(~>) :: a -> a -> CTest a
(~>) = flip (<~)

-- identical sets
(<~>) :: a -> a -> CTest a
(<~>) = SetTest

--- Type of IO assertions.
data PropIO = PropIO (String -> IO (Maybe String))

-- IO tests
-- the IO operation yields a specified result
yields :: IO a -> a -> PropIO
yields act r = PropIO (execIOTest act (return r))

-- two IO operations yield the same result
sameAs :: IO a -> IO a -> PropIO
sameAs a1 a2 = PropIO (execIOTest a1 a2)

-- helper functions
-- non-deterministically return any value of the list
anyOf :: [a] -> a
anyOf = foldr1 (?)

-- non-deterministically return any value satisfying the constraint
anySolutionOf :: (a -> Success) -> a
anySolutionOf c | c x = x
 where x free

-------------------------------------------------------------------------
-- internal functions used by CurryCheck to check an IO assertion
execAssertIO :: PropIO -> String -> IO (Maybe String)
execAssertIO p msg = catchNDIO msg $
  case p of PropIO propio -> propio msg

execIOTest :: IO a -> IO a -> String -> IO (Maybe String)
execIOTest act1 act2 msg =
   catch (do putStr (msg++": ") >> hFlush stdout
             r1 <- act1
             r2 <- act2
             if r1 == r2 then putStrLn "OK" >>  return Nothing
                         else putStrLn "FAILED!" >> return (Just msg)
         )
         (\err -> do putStrLn $ "EXECUTION FAILURE:\n" ++ showError err
                     return (Just msg)
         )

-- Execute I/O action for assertion checking and report any failure
-- or non-determinism.
catchNDIO :: String -> IO (Maybe String) -> IO (Maybe String)
catchNDIO msg testact =
  if curryCompiler == "kics2"
  then -- specific handling for KiCS2 since it might report non-det errors
       -- even if there is only one result value, e.g., in functional patterns
       getAllValues testact >>= checkIOActions
  else catch testact
             (\e -> putStrLn (msg++": EXECUTION FAILURE: "++showError e) >>
                    return (Just msg))
 where
  checkIOActions results
    | null results
     = putStrLn (msg++": FAILURE: computation failed") >> return (Just msg)
    | not (null (tail results))
     = putStrLn (msg++": FAILURE: computation is non-deterministic") >>
       return (Just msg)
    | otherwise = head results

-- transform a CTest into a property
makeProp :: CTest _ -> Test.EasyCheck.Prop
makeProp (CondTest   p t) = p Test.EasyCheck.==> makeProp t
makeProp (EqualTest  a b) = a Test.EasyCheck.<=> b
makeProp (SubSetTest a b) = a Test.EasyCheck.<~  b
makeProp (SetTest    a b) = a Test.EasyCheck.<~> b

execPropWithMsg :: String -> IO Bool -> IO (Maybe String)
execPropWithMsg msg execprop = do
  b <- execprop
  return (if b then Nothing else Just msg)

-- Runs a sequence of tests and
-- yields a new exit status based on the succesfully executed tests.
execProps :: [IO (Maybe String)] -> IO Int
execProps props = do
  propresults <- sequenceIO props
  let failedmsgs = catMaybes propresults
  if null failedmsgs
   then return 0
   else do putStrLn $ line ++
                      "\nFAILURE OCCURRED IN SOME TESTS:\n" ++
                      unlines failedmsgs ++ line
           return 1
 where
   line = take 78 (repeat '=')
