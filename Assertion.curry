------------------------------------------------------------------------------
--- This module defines the datatype and operations for the
--- Curry module tester "currytest".
---
--- @author Michael Hanus
--- @version January 2015
------------------------------------------------------------------------------

{-# OPTIONS_CYMAKE -X TypeClassExtensions #-}

module Assertion(-- for writing test cases:
                 Assertion,assertTrue,assertEqual,
                 assertValues,assertSolutions,assertIO,assertEqualIO,
                 -- the remaining entities are only used by the test tool:
                 checkAssertion,
                 seqStrActions,writeAssertResult,
                 ProtocolMsg(..),
                 showTestMod,showTestCase,showTestEnd,showTestCompileError)
 where

import AllSolutions
import List((\\))
import Socket -- for sending results to test GUI
import IO(hPutStrLn,hClose)
import Distribution(curryCompiler)

infixl 1 `seqStrActions`

--- Datatype of assertions.
--- Internally, each assertion consists of a name and an I/O action
--- to check the validity of the assertion.
data Assertion = Assert String (IO (String,Bool))

--- `(assertTrue s b)` asserts (with name `s`) that `b` must be true.
assertTrue :: String -> Bool -> Assertion
assertTrue s b = Assert s (checkAssertTrue s b)

--- `(assertEqual s e1 e2)` asserts (with name `s`) that `e1` and `e2`
--- must be equal (w.r.t. `==`).
assertEqual :: (Show a,Eq a) => String -> a -> a -> Assertion
assertEqual s x y = Assert s (checkAssertEqual s x y)

--- `(assertValues s e vs)` asserts (with name `s`) that `vs` is the multiset
--- of all values of `e`. All values of `e` are
--- compared with the elements in `vs` w.r.t. `==`.
assertValues :: (Show a,Eq a) => String -> a -> [a] -> Assertion
assertValues s x y = Assert s (checkAssertValues s x y)

--- `(assertSolutions s c vs)` asserts (with name `s`) that constraint
--- abstraction `c` has the multiset of solutions `vs`.
--- The solutions of `c` are compared with the elements in `vs` w.r.t. `==`.
assertSolutions :: (Show a,Eq a) => String -> (a->Success) -> [a] -> Assertion
assertSolutions s x y = Assert s (checkAssertSolutions s x y)

--- `(assertIO s a r)` asserts (with name `s`) that I/O action `a`
--- yields the result value `r`.
assertIO :: (Show a,Eq a) => String -> IO a -> a -> Assertion
assertIO s x y = Assert s (checkAssertIO s x y)

--- `(assertEqualIO s a1 a2)` asserts (with name `s`) that I/O actions `a1`
--- and `a2` yield equal (w.r.t. `==`) results.
assertEqualIO :: (Show a,Eq a) => String -> IO a -> IO a -> Assertion
assertEqualIO s x y = Assert s (checkAssertEqualIO s x y)


--- Combines two actions and combines their results.
--- Used by the currytest tool.
seqStrActions :: IO (String,Bool) -> IO (String,Bool) -> IO (String,Bool)
seqStrActions a1 a2 =
  do (s1,b1) <- a1
     (s2,b2) <- a2
     return (s1++s2,b1&&b2)

--- Executes and checks an assertion, and process the result
--- by an I/O action.
--- Used by the currytest tool.
--- @param name      - the name of the operation defining the assertion
--- @param protocol  - an action to be applied after test execution
--- @param assertion - an assertion to be tested
--- @return a protocol string and a flag whether the test was successful
checkAssertion :: String -> ((String,Bool) -> IO (String,Bool))
               -> Assertion -> IO (String,Bool)
checkAssertion asrtop prot (Assert asrtname asrtact) =
  catchNDIO asrtop prot (catch asrtact returnError >>= prot)
 where
  returnError err =
    return ("FAILURE of "++asrtname++": "++showError err++"\n",False)
  
-- Execute I/O action for assertion checking and report any failure
-- or non-determinism.
catchNDIO :: String -> ((String,Bool) -> IO (String,Bool))
          -> IO (String,Bool) -> IO (String,Bool)
catchNDIO fname prot a =
  if curryCompiler == "kics2"
  then -- specific handling for KiCS2 since it might report non-det errors
       -- even if there is only one result value, e.g., in functional patterns
       getAllValues a >>= checkIOActions
  else catch a (\e -> prot ("ERROR in "++fname++": "++showError e++"\n",False))
 where
  checkIOActions results
    | null results
     = prot ("ERROR in "++fname++": computation failed\n",False)
    | not (null (tail results))
     = prot ("ERROR in "++fname++
             ": computation is non-deterministic\n",False)
    | otherwise = head results

-- Checks Boolean assertion.
checkAssertTrue :: String -> Bool -> IO (String,Bool)
checkAssertTrue name cond = catchNDIO name return $
  if cond
  then return ("OK: "++name++"\n",True)
  else return ("FAILURE of "++name++": assertion not satisfied:\n",False)

-- Checks equality assertion.
checkAssertEqual :: (Show a,Eq a) => String -> a -> a -> IO (String,Bool)
checkAssertEqual name call result = catchNDIO name return $
  if call==result
  then return ("OK: "++name++"\n",True)
  else return ("FAILURE of "++name++": equality assertion not satisfied:\n"++
               "Computed answer: "++show call++"\n"++
               "Expected answer: "++show result++"\n",False)

-- Checks all values assertion.
checkAssertValues :: (Show a, Eq a) => String -> a -> [a] -> IO (String,Bool)
checkAssertValues name call results = do
  rs <- getAllValues call
  if null (rs \\ results) && null (results \\ rs)
   then return ("OK: "++name++"\n",True)
   else return ("FAILURE of "++name++": values assertion not satisfied:\n"++
                "Computed values: "++show rs++"\n"++
                "Expected values: "++show results++"\n",False)

-- Checks all solutions of a constraint abstraction.
checkAssertSolutions :: (Show a, Eq a) => String -> (a->Success) -> [a] -> IO (String,Bool)
checkAssertSolutions name constr results = do
  rs <- getAllSolutions constr
  if null (rs \\ results) && null (results \\ rs)
   then return ("OK: "++name++"\n",True)
   else return ("FAILURE of "++name++": solutions assertion not satisfied:\n"++
                "Computed values: "++show rs++"\n"++
                "Expected values: "++show results++"\n",False)

-- Checks an IO assertion.
checkAssertIO :: (Show a, Eq a) => String -> IO a -> a -> IO (String,Bool)
checkAssertIO name action result = do
  r <- action
  if r==result
    then return ("OK: "++name++"\n",True)
    else return ("FAILURE of "++name++": IO assertion not satisfied:\n"++
                 "Computed answer: "++show r++"\n"++
                 "Expected answer: "++show result++"\n\n",False)

-- Checks equality of results of two IO assertions.
checkAssertEqualIO :: (Show a, Eq a) =>
                      String -> IO a -> IO a -> IO (String,Bool)
checkAssertEqualIO name action1 action2 = do
  r1 <- action1
  r2 <- action2
  if r1==r2
    then return ("OK: "++name++"\n",True)
    else return ("FAILURE of "++name++": IO equality assertion not satisfied:\n"++
                 "Computed answer 1: "++show r1++"\n"++
                 "Computed answer 2: "++show r2++"\n\n",False)

--- Prints the results of assertion checking.
--- If failures occurred, the return code is positive.
--- Used by the currytest tool.
writeAssertResult :: (String,Bool) -> IO Int
writeAssertResult (result,flag) =
  if flag
  then putStrLn (result++"All tests successfully passed.")         >> return 0
  else putStrLn (result++"FAILURE occurred in some assertions!\n") >> return 1


----------------------------------------------------------------------------
-- The following entities are used to implement the test GUI:

--- The messages sent to the test GUI.
--- Used by the currytest tool.
data ProtocolMsg = TestModule String | TestCase String Bool | TestFinished
                 | TestCompileError
  deriving Show

--- Sends message to GUI for showing test of a module.
--- Used by the currytest tool.
showTestMod :: Int -> String -> IO ()
showTestMod portnum modname = sendToLocalSocket portnum (TestModule modname)

--- Sends message to GUI for showing result of executing a test case.
--- Used by the currytest tool.
showTestCase :: Int -> (String,Bool) -> IO (String,Bool)
showTestCase portnum (s,b) = do
  sendToLocalSocket portnum (TestCase s b)
  return (s,b)

--- Sends message to GUI for showing end of module test.
--- Used by the currytest tool.
showTestEnd :: Int -> IO ()
showTestEnd portnum = sendToLocalSocket portnum TestFinished

--- Sends message to GUI for showing compilation errors in a module test.
--- Used by the currytest tool.
showTestCompileError :: Int -> IO ()
showTestCompileError portnum = sendToLocalSocket portnum TestCompileError

--- Sends protocol message to local socket.
sendToLocalSocket :: Int -> ProtocolMsg -> IO ()
sendToLocalSocket portnum msg = do
  h <- connectToSocket "localhost" portnum
  hPutStrLn h (show msg)
  hClose h

-- end of module Assertion
