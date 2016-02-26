-------------------------------------------------------------------------
--- EasyCheck is a library for automated, specification-based testing of
--- Curry programs. The ideas behind EasyCheck are described in
--- [this paper](http://www-ps.informatik.uni-kiel.de/~sebf/pub/flops08.html)
--- The tool `currycheck` automatically executes tests defined with
--- this library. EasyCheck supports the definition of unit tests
--- (also for I/O operations) and property tests parameterized
--- over some arguments. The latter kind of tests can only be executed
--- with KiCS2.
---
--- @author Sebastian Fischer (with extensions by Michael Hanus)
--- @version February 2016
--- @category general
-------------------------------------------------------------------------

module Test.EasyCheck (

  -- test specification:
  PropIO, returns, sameReturns,

  Config, Test, Prop, (==>), for,

  test, is, isAlways, isEventually, uniquely, always, eventually,
  failing, successful, deterministic, (-=-), (#), (<~>), (~>), (<~), (<~~>),
  solutionOf,

  -- test annotations
  label, trivial, classify, collect, collectAs,

  -- configurations
  verboseConfig, quietConfig, easyConfig, setMaxTest, setMaxFail,

  -- test functions
  checkWithValues0, checkWithValues1, checkWithValues2,
  checkWithValues3, checkWithValues4, checkWithValues5,
  check0, check1, check2, check3, check4, check5,
  easyCheck0, easyCheck1, easyCheck2, easyCheck3, easyCheck4, easyCheck5,
  verboseCheck0, verboseCheck1, verboseCheck2, verboseCheck3, verboseCheck4,
  verboseCheck5,

  valuesOfSearchTree, valuesOf, Result(..), result,

  -- easyCheck', easyCheck1', easyCheck2', easyCheck3', easyCheck4', easyCheck5',

  -- operations used by the CurryCheck tool
  checkPropWithMsg, checkPropIOWithMsg, runPropertyTests
  ) where

import AllSolutions ( getAllValues )
import Distribution ( curryCompiler )
import List         ( (\\), delete, diagonal, group, intersperse, nub )
import Maybe        ( catMaybes )
import SearchTree   ( SearchTree, someSearchTree )
import SearchTreeTraversal
import Sort         ( leqList, leqString, mergeSort )

infix  4 `isSameSet`, `isSubsetOf`, `isSameMSet`
infix  1 `is`, `isAlways`, `isEventually`, -=-, #, <~>, ~>, <~, <~~>, `trivial`
infix  1 `returns`, `sameReturns`
infixr 0 ==>


-------------------------------------------------------------------------
--- Abstract type to represent properties involving IO actions.
data PropIO = PropIO (Config -> String -> IO (Maybe String))

--- The property `returns a x` is satisfied if the execution of the
--- I/O action `a` returns the value `x`.
returns :: IO a -> a -> PropIO
returns act r = PropIO (testIO act (return r))

--- The property `sameReturns a1 a2` is satisfied if the execution of the
--- I/O actions `a1` and `a2` return identical values.
sameReturns :: IO a -> IO a -> PropIO
sameReturns a1 a2 = PropIO (testIO a1 a2)

-------------------------------------------------------------------------
--- Abstract type to a single test for a property to be checked.
data Test = Test Result [String] [String]

--- Data type to represent the result of checking a property.
data Result = Undef | Ok | Falsified [String] | Ambigious [Bool] [String]

--- Abstract type to represent properties to be checked.
type Prop = [Test]

notest :: Test
notest = Test Undef [] []

--- Extracts the result of a single test.
result :: Test -> Result
result (Test r _ _) = r

setResult :: Result -> Test -> Test
setResult res (Test _ s a) = Test res a s

args :: Test -> [String]
args  (Test _ a _) = a

stamp :: Test -> [String]
stamp (Test _ _ s) = s

updArgs :: ([String] -> [String]) -> Test -> Test
updArgs  upd (Test r a s) = Test r (upd a) s

updStamp :: ([String] -> [String]) -> Test -> Test
updStamp upd (Test r a s) = Test r a (upd s)

-- Test Specification

--- Constructs a property to be tested from an arbitrary expression
--- (first argument) and a predicate that is applied to the list of
--- non-deterministic values. The given predicate determines whether
--- the constructed property is satisfied or falsified for the given
--- expression.
test :: a -> ([a] -> Bool) -> Prop
test x f = [setResult res notest]
 where
  xs  = valuesOf x
  res = case valuesOf (f xs) of
          [True]  -> Ok
          [False] -> Falsified (map show xs)
          bs      -> Ambigious bs (map show xs)

--- The property `x -=- y` is satisfied if `x` and `y` have deterministic
--- values that are equal.
(-=-) :: a -> a -> Prop
x -=- y = (x,y) `is` uncurry (==)

--- The property `x <~> y` is satisfied if the sets of the values of
--- `x` and `y` are equal.
(<~>) :: a -> a -> Prop
x <~>  y = test x (isSameSet (valuesOf y))

--- The property `x ~> y` is satisfied if `x` evaluates to every value of `y`.
--- Thus, the set of values of `y` must be a subset of the set of values of `x`.
(~>) :: a -> a -> Prop
x  ~>  y = test x (isSubsetOf (valuesOf y))

--- The property `x <~ y` is satisfied if `y` evaluates to every value of `x`.
--- Thus, the set of values of `x` must be a subset of the set of values of `y`.
(<~) :: a -> a -> Prop
x  <~  y = test x (`isSubsetOf` (valuesOf y))

--- The property `x <~~> y` is satisfied if the multisets of the values of
--- `x` and `y` are equal.
(<~~>) :: a -> a -> Prop
x <~~> y = test x (isSameMSet (valuesOf y))

isSameSet :: [a] -> [a] -> Bool
xs `isSameSet` ys = xs' `subset` ys' && ys' `subset` xs'
 where xs' = nub xs; ys' = nub ys

isSubsetOf :: [a] -> [a] -> Bool
xs `isSubsetOf` ys = nub xs `subset` ys

subset :: [a] -> [a] -> Bool
xs `subset` ys = null (xs\\ys)

-- compare to lists if they represent the same multi-set
isSameMSet :: [a] -> [a] -> Bool
[]     `isSameMSet` ys = ys == []
(x:xs) `isSameMSet` ys
  | x `elem` ys        = xs `isSameMSet` (delete x ys)
  | otherwise          = False

--- A conditional property is tested if the condition evaluates to `True`.
(==>) :: Bool -> Prop -> Prop
cond ==> p =
  if True `elem` valuesOf cond
  then p
  else [notest]

--- `solutionOf p` returns (non-deterministically) a solution
--- of predicate `p`. This operation is useful to test solutions
--- of predicates.
solutionOf :: (a -> Bool) -> a
solutionOf pred = pred x &> x where x free

--- The property `is x p` is satisfied if `x` has a deterministic value
--- which satisfies `p`.
is :: a -> (a -> Bool) -> Prop
is x f = test x (\xs -> case xs of [y] -> f y; _ -> False)

--- The property `isAlways x p` is satisfied if all values of `x` satisfy `p`.
isAlways :: a -> (a -> Bool) -> Prop
isAlways x  = test x . all

--- The property `isEventually x p` is satisfied if some value of `x`
--- satisfies `p`.
isEventually :: a -> (a -> Bool) -> Prop
isEventually x = test x . any

--- The property `uniquely x` is satisfied if `x` has a deterministic value
--- which is true.
uniquely :: Bool -> Prop
uniquely = (`is` id)

--- The property `always x` is satisfied if all values of `x` are true.
always :: Bool -> Prop
always = (`isAlways` id)

--- The property `eventually x` is satisfied if some value of `x` is true.
eventually :: Bool -> Prop
eventually = (`isEventually` id)

--- The property `failing x` is satisfied if `x` has no value.
failing :: _ -> Prop
failing x = test x null

--- The property `successful x` is satisfied if `x` has at least one value.
successful :: _ -> Prop
successful x = test x (not . null)

--- The property `deterministic x` is satisfied if `x` has exactly one value.
deterministic :: _ -> Prop
deterministic x = x `is` const True

--- The property `x # n` is satisfied if `x` has `n` values.
(#) :: _ -> Int -> Prop
x # n = test x ((n==) . length . nub)

forAll :: (b -> Prop) -> a -> (a -> b) -> Prop
forAll c x f =
  diagonal [[ updArgs (show y:) t | t <- c (f y) ] | y <- valuesOf x ]

forAllValues :: (b -> Prop) -> [a] -> (a -> b) -> Prop
forAllValues c vals f =
  diagonal [[ updArgs (show y:) t | t <- c (f y) ] | y <- vals ]

--- The property `for x p` is satisfied if all values `y` of `x`
--- satisfy property `p x`.
for :: a -> (a -> Prop) -> Prop
for x p = forAllValues id (valuesOf x) p

--- The property `for x p` is satisfied if all values `y` of `x`
--- satisfy property `p x`.
forValues :: [a] -> (a -> Prop) -> Prop
forValues xs p = forAllValues id xs p

-------------------------------------------------------------------------
-- Test Annotations

label :: String -> Prop -> Prop
label = map . updStamp . (:)

classify :: Bool -> String -> Prop -> Prop
classify True  name = label name
classify False _    = id

trivial :: Bool -> Prop -> Prop
trivial = (`classify` "trivial")

collect :: a -> Prop -> Prop
collect = label . show

collectAs :: String -> a -> Prop -> Prop
collectAs name = label . ((name++": ")++) . show

-------------------------------------------------------------------------
--- The configuration of property testing.
--- The configuration contains
---  * the maximum number of tests,
---  * the maximum number of condition failures before giving up,
---  * an operation that shows the number and arguments of each test,
---  * a status whether it should work quietly.
data Config = Config
  { maxTest :: Int
  , maxFail :: Int
  , every   :: Int -> [String] -> String
  , isQuiet :: Bool
  }

--- Sets the maximum number of tests in a test configuration.
setMaxTest :: Int -> Config -> Config
setMaxTest n config = config { maxTest = n }

--- Sets the maximum number of condition failures in a test configuration.
setMaxFail :: Int -> Config -> Config
setMaxFail n config = config { maxFail = n }

--- The default configuration for EasyCheck shows and deletes the number
--- for each test.
easyConfig :: Config
easyConfig =
 Config { maxTest = 100
        , maxFail = 10000
        , every = (\n _ -> let s = ' ':show (n+1) in s ++ [ chr 8 | _ <- s ])
        , isQuiet = False
        }

--- A verbose configuration which shows the arguments of every test.
verboseConfig :: Config
verboseConfig = easyConfig { every = (\n xs -> show n ++ ":\n" ++ unlines xs) }

--- A quiet configuration which shows nothing but failed tests.
quietConfig :: Config
quietConfig = easyConfig { isQuiet = True, every = (\_ _ -> "") }

-------------------------------------------------------------------------
-- Test Functions

suc :: (a -> Prop) -> (b -> a) -> Prop
suc n =
  if curryCompiler == "kics2"
  then forAllValues n (valuesOf unknown)
  else error
    "Test.EasyCheck: tests with arbitrary values not yet implemented in PAKCS!"

--- Checks a unit test with a given configuration (first argument)
--- and a name for the test (second argument).
--- Returns a flag whether the test was successful.
check0 :: Config -> String -> Prop -> IO Bool
check0 = check

--- Checks a unit test with a given configuration (first argument)
--- and a name for the test (second argument).
--- Returns a flag whether the test was successful.
checkWithValues0 :: Config -> String -> Prop -> IO Bool
checkWithValues0 = check

--- Checks a property parameterized over a single argument
--- with a given configuration (first argument),
--- a name for the test (second argument),
--- and all values given in the third argument.
--- Returns a flag whether the test was successful.
checkWithValues1 :: Config -> String -> [a] -> (a -> Prop) -> IO Bool
checkWithValues1 config msg xs p = check config msg (forValues xs p)

--- Checks a property parameterized over two arguments
--- with a given configuration (first argument)
--- a name for the test (second argument),
--- and all values given in the third and fourth argument.
--- Returns a flag whether the test was successful.
checkWithValues2 :: Config -> String -> [a] -> [b]
                -> (a -> b -> Prop) -> IO Bool
checkWithValues2 config msg xs ys p =
  check config msg (forValues xs (\x -> forValues ys (p x)))

--- Checks a property parameterized over three arguments
--- with a given configuration (first argument)
--- a name for the test (second argument),
--- and all values given in the third, fourth and fifth argument.
--- Returns a flag whether the test was successful.
checkWithValues3 :: Config -> String -> [a] -> [b] -> [c]
                -> (a -> b -> c -> Prop) -> IO Bool
checkWithValues3 config msg xs ys zs p =
  check config msg
        (forValues xs (\x -> forValues ys (\y -> forValues zs (p x y))))

--- Checks a property parameterized over four arguments
--- with a given configuration (first argument)
--- a name for the test (second argument),
--- and all values given in the further arguments.
--- Returns a flag whether the test was successful.
checkWithValues4 :: Config -> String -> [a] -> [b] -> [c] -> [d]
                -> (a -> b -> c -> d -> Prop) -> IO Bool
checkWithValues4 config msg xs ys zs1 zs2 p =
  check config msg
        (forValues xs (\x -> forValues ys
                      (\y -> forValues zs1
                      (\z1 -> forValues zs2 (p x y z1)))))

--- Checks a property parameterized over five arguments
--- with a given configuration (first argument)
--- a name for the test (second argument),
--- and all values given in the further arguments.
--- Returns a flag whether the test was successful.
checkWithValues5 :: Config -> String -> [a] -> [b] -> [c] -> [d] -> [e]
                -> (a -> b -> c -> d -> e -> Prop) -> IO Bool
checkWithValues5 config msg xs ys zs1 zs2 zs3 p =
  check config msg
        (forValues xs (\x -> forValues ys
                      (\y -> forValues zs1
                      (\z1 -> forValues zs2
                      (\z2 -> forValues zs3 (p x y z1 z2))))))

--- Checks a property parameterized over a single argument
--- with a given configuration (first argument)
--- and a name for the test (second argument).
--- Returns a flag whether the test was successful.
check1 :: Config -> String -> (_ -> Prop) -> IO Bool
check1 config msg = check config msg . suc id

--- Checks a property parameterized over two arguments
--- with a given configuration (first argument)
--- and a name for the test (second argument).
--- Returns a flag whether the test was successful.
check2 :: Config -> String -> (_ -> _ -> Prop) -> IO Bool
check2 config msg = check config msg . suc (suc id)

--- Checks a property parameterized over three arguments
--- with a given configuration (first argument)
--- and a name for the test (second argument).
--- Returns a flag whether the test was successful.
check3 :: Config -> String -> (_ -> _ -> _ -> Prop) -> IO Bool
check3 config msg = check config msg . suc (suc (suc id))

--- Checks a property parameterized over four arguments
--- with a given configuration (first argument)
--- and a name for the test (second argument).
--- Returns a flag whether the test was successful.
check4 :: Config -> String -> (_ -> _ -> _ -> _ -> Prop) -> IO Bool
check4 config msg = check config msg . suc (suc (suc (suc id)))

--- Checks a property parameterized over five arguments
--- with a given configuration (first argument)
--- and a name for the test (second argument).
--- Returns a flag whether the test was successful.
check5 :: Config -> String -> (_ -> _ -> _ -> _ -> _ -> Prop) -> IO Bool
check5 config msg = check config msg . suc (suc (suc (suc (suc id))))


--- Checks a unit test according to the default configuration
--- and a name for the test (first argument).
--- Returns a flag whether the test was successful.
easyCheck0 :: String -> Prop -> IO Bool
easyCheck0 = check0 easyConfig

--- Checks a property parameterized over a single argument
--- according to the default configuration
--- and a name for the test (first argument).
--- Returns a flag whether the test was successful.
easyCheck1 :: String -> (_ -> Prop) -> IO Bool
easyCheck1 = check1 easyConfig

--- Checks a property parameterized over two arguments
--- according to the default configuration
--- and a name for the test (first argument).
--- Returns a flag whether the test was successful.
easyCheck2 :: String -> (_ -> _ -> Prop) -> IO Bool
easyCheck2 = check2 easyConfig

--- Checks a property parameterized over three arguments
--- according to the default configuration
--- and a name for the test (first argument).
--- Returns a flag whether the test was successful.
easyCheck3 :: String -> (_ -> _ -> _ -> Prop) -> IO Bool
easyCheck3 = check3 easyConfig

--- Checks a property parameterized over four arguments
--- according to the default configuration
--- and a name for the test (first argument).
--- Returns a flag whether the test was successful.
easyCheck4 :: String -> (_ -> _ -> _ -> _ -> Prop) -> IO Bool
easyCheck4 = check4 easyConfig

--- Checks a property parameterized over five arguments
--- according to the default configuration
--- and a name for the test (first argument).
--- Returns a flag whether the test was successful.
easyCheck5 :: String -> (_ -> _ -> _ -> _ -> _ -> Prop) -> IO Bool
easyCheck5 = check5 easyConfig

--- Checks a unit test according to the verbose configuration
--- and a name for the test (first argument).
--- Returns a flag whether the test was successful.
verboseCheck0 :: String -> Prop -> IO Bool
verboseCheck0 = check0 verboseConfig

--- Checks a property parameterized over a single argument
--- according to the verbose configuration
--- and a name for the test (first argument).
--- Returns a flag whether the test was successful.
verboseCheck1 :: String -> (_ -> Prop) -> IO Bool
verboseCheck1 = check1 verboseConfig

--- Checks a property parameterized over two arguments
--- according to the verbose configuration
--- and a name for the test (first argument).
--- Returns a flag whether the test was successful.
verboseCheck2 :: String -> (_ -> _ -> Prop) -> IO Bool
verboseCheck2 = check2 verboseConfig

--- Checks a property parameterized over three arguments
--- according to the verbose configuration
--- and a name for the test (first argument).
--- Returns a flag whether the test was successful.
verboseCheck3 :: String -> (_ -> _ -> _ -> Prop) -> IO Bool
verboseCheck3 = check3 verboseConfig

--- Checks a property parameterized over four arguments
--- according to the verbose configuration
--- and a name for the test (first argument).
--- Returns a flag whether the test was successful.
verboseCheck4 :: String -> (_ -> _ -> _ -> _ -> Prop) -> IO Bool
verboseCheck4 = check4 verboseConfig

--- Checks a property parameterized over five arguments
--- according to the verbose configuration
--- and a name for the test (first argument).
--- Returns a flag whether the test was successful.
verboseCheck5 :: String -> (_ -> _ -> _ -> _ -> _ -> Prop) -> IO Bool
verboseCheck5 = check5 verboseConfig


check :: Config -> String -> Prop -> IO Bool
check config msg p = tests config msg p 0 0 []

tests :: Config -> String -> [Test] -> Int -> Int -> [[String]] -> IO Bool
tests config msg [] ntest _ stamps =
  done config (msg ++ ":\n Passed") ntest stamps True
tests config msg (t:ts) ntest nfail stamps
  | ntest == maxTest config
  = done config (msg ++ ":\n OK, passed") ntest stamps True
  | nfail == maxFail config
  = done config (msg ++ ":\n Arguments exhausted after") ntest stamps False
  | otherwise = do
      putStr (every config ntest (args t))
      case result t of
        Undef -> tests config msg ts ntest (nfail+1) stamps
        Ok    -> tests config msg ts (ntest+1) nfail (stamp t:stamps)
        Falsified results -> do
          putStr $
            msg ++ " failed\n" ++
            "Falsified by " ++ nth (ntest+1) ++ " test" ++
            (if null (args t) then "." else ".\nArguments:") ++ "\n" ++
            unlines (args t) ++
            if null results then "no result\n"
              else "Results:\n" ++ unlines (nub results)
          return False
        Ambigious bs results -> do
          putStr $
            "Ambigious property yields " ++ show bs ++ " for " ++
            nth (ntest+1) ++ " test" ++
            (if null (args t) then "." else ".\nArguments:") ++ "\n" ++
            unlines (args t) ++
            if null results then "no result\n"
              else "Results:\n" ++ unlines (nub results)
          return False

check' :: Config -> Prop -> Result
check' config p = tests' config p 0 0 []

tests' :: Config -> [Test] -> Int -> Int -> [[String]] -> Result
tests' config (t:ts) ntest nfail stamps
  | ntest == maxTest config = Ok
  | nfail == maxFail config = Falsified ["Arguments exhausted after " ++ show ntest ++ " test"]
  | otherwise = case result t of
                     Undef     -> tests' config ts ntest (nfail+1) stamps
                     Ok        -> tests' config ts (ntest+1) nfail stamps
                     res       -> res

easyCheck' :: Prop -> Result
easyCheck' = check' easyConfig

easyCheck1' :: (_ -> Prop) -> Result
easyCheck1' = easyCheck' . suc id

easyCheck2' :: (_ -> _ -> Prop) -> Result
easyCheck2' = easyCheck' . suc (suc id)

easyCheck3' :: (_ -> _ -> _ -> Prop) -> Result
easyCheck3' = easyCheck' . suc (suc (suc id))

easyCheck4' :: (_ -> _ -> _ -> _ -> Prop) -> Result
easyCheck4' = easyCheck' . suc (suc (suc (suc id)))

easyCheck5' :: (_ -> _ -> _ -> _ -> _ -> Prop) -> Result
easyCheck5' = easyCheck' . suc (suc (suc (suc (suc id))))

nth :: Int -> String
nth n = case n of 1 -> "first"; 2 -> "second"; 3 -> "third"; _ -> show n++ "th"

done :: Config -> String -> Int -> [[String]] -> Bool -> IO Bool
done config mesg ntest stamps status = do
  unless (isQuiet config) $
    putStr (mesg ++ " " ++ show ntest ++ " test"
            ++ (if ntest >= 2 then "s" else "") ++ table)
  return status
 where
  table = display
        . map entry
        . reverse
        . mergeSort (leqPair (<=) (leqList leqString))
        . map pairLength
        . group
        . mergeSort (leqList leqString)
        . filter (not . null)
        $ stamps

  display []         = ".\n"
  display [x]        = " - " ++ x ++ ".\n"
  display xs@(_:_:_) = ".\n" ++ unlines (map (++".") xs)

  pairLength xss@(xs:_) = (length xss,xs)

  entry (n,xs) = percentage n ntest ++ " " ++ concat (intersperse ", " xs)

  percentage n _ = let s = show n -- ((100*n)`div`m)
                    in replicate (5-length s) ' ' ++ s -- ++ "%"

-- Auxiliary Functions

leqPair :: (a -> a -> Bool) -> (b -> b -> Bool) -> ((a,b) -> (a,b) -> Bool)
leqPair leqa leqb (x1,y1) (x2,y2)
  | x1 == x2  = leqb y1 y2
  | otherwise = leqa x1 x2

--- Extracts values of a search tree according to a given strategy
--- (here: randomized diagonalization of levels with flattening).
valuesOfSearchTree :: SearchTree a -> [a]
valuesOfSearchTree
  -- = depthDiag            
  -- = rndDepthDiag 0       
  -- = levelDiag            
  -- = rndLevelDiag 0       
  = rndLevelDiagFlat 5 0 
  -- = allValuesB           

--- Computes the list of all values of the given argument
--- according to a given strategy (here:
--- randomized diagonalization of levels with flattening).
valuesOf :: a -> [a]
valuesOf = valuesOfSearchTree . someSearchTree . (id $##)

-------------------------------------------------------------------------
--- Safely checks a property, i.e., catch all exceptions that might occur
--- and return appropriate error message in case of a failed test.
checkPropWithMsg :: String -> IO Bool -> IO (Maybe String)
checkPropWithMsg msg execprop = catchNDIO msg $ do
  b <- catch execprop
             (\e -> putStrLn (msg ++ ": EXECUTION FAILURE:\n" ++ showError e)
                    >> return False)
  return (if b then Nothing else Just msg)

--- Safely checks an IO property, i.e., catch all exceptions that might occur
--- and return appropriate error message in case of a failed test.
--- This operation is used by the currycheck tool.
checkPropIOWithMsg :: Config -> String -> PropIO -> IO (Maybe String)
checkPropIOWithMsg config msg p = catchNDIO msg $
  case p of PropIO propio -> propio config msg

-- Test an IO property, i.e., compare the results of two IO actions.
testIO :: IO a -> IO a -> Config -> String -> IO (Maybe String)
testIO act1 act2 config msg =
   catch (do r1 <- act1
             r2 <- act2
             if r1 == r2
               then putStrNQ (msg++": OK\n") >>  return Nothing
               else do putStrLn $ msg++": FAILED!\nResults: " ++ show (r1,r2)
                       return (Just msg)
         )
         (\err -> do putStrLn $ msg++": EXECUTION FAILURE:\n" ++ showError err
                     return (Just msg)
         )
 where
  putStrNQ = unless (isQuiet config) . putStr

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

--- Runs a sequence of property tests. Outputs the messages of the failed tests
--- messages and returns exit status 0 if all tests are successful,
--- otherwise status 1.
--- This operation is used by the currycheck tool.
runPropertyTests :: [IO (Maybe String)] -> IO Int
runPropertyTests props = do
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

-------------------------------------------------------------------------
