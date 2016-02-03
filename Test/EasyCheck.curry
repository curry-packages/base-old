-------------------------------------------------------------------------
--- EasyCheck is a library for automated, specification-based testing of
--- Curry programs. The ideas behind EasyCheck are described in
--- [this paper](http://www-ps.informatik.uni-kiel.de/~sebf/pub/flops08.html)
--- The tool `currycheck` automatically executes tests defined with
--- this library. EasyCheck supports the definition of unit tests
--- (also for I/O operations) and property tests parameterized
--- over some arguments. The latter kind of tests can only be tested
--- with KiCS2.
---
--- @author Sebastian Fischer (with extensions by Michael Hanus)
--- @version February 2016
-------------------------------------------------------------------------

module Test.EasyCheck (

  -- test specification:
  PropIO, yields, sameAs,

  Test, Prop, (==>), for,

  test, is, isAlways, isEventually, prop, uniquely, always, eventually,
  failing, successful, deterministic, (-=-), (#), (<~>), (~>), (<~), (<~~>),

  -- test annotations
  label, trivial, classify, collect, collectAs,

  -- configurations
  verboseConfig, quietConfig, easyConfig, setMaxTest, setMaxFail, setEvery,

  -- test functions
  configCheck0, configCheck1, configCheck2, configCheck3, configCheck4, configCheck5,
  easyCheck, easyCheck0, easyCheck1, easyCheck2, easyCheck3, easyCheck4, easyCheck5,
  verboseCheck, verboseCheck0, verboseCheck1, verboseCheck2, verboseCheck3, verboseCheck4,
  verboseCheck5,

  valuesOf, Result(..), result,

  easyCheck', easyCheck1', easyCheck2', easyCheck3', easyCheck4', easyCheck5',

  -- internal operations used by by the CurryCheck tool
  checkPropIO, execPropWithMsg, execProps
  ) where

import AllSolutions ( getAllValues )
import Distribution ( curryCompiler )
import IO           ( hFlush, stdout )
import List         ( delete, nub, group, intersperse, (\\) )
import Maybe        ( catMaybes )
import Random       ( nextInt )
import SearchTree   ( someSearchTree )
import SearchTreeTraversal
import Sort         ( leqList, leqString, mergeSort )

infix  4 `isSameSet`, `isSubsetOf`, `isSameMSet`
infix  1 `is`, `isAlways`, `isEventually`, -=-, #, <~>, ~>, <~, <~~>, `trivial`
infix  1 `yields`, `sameAs`
infixr 0 ==>


-------------------------------------------------------------------------
--- Type of IO assertions.
data PropIO = PropIO (Config -> String -> IO (Maybe String))

-- IO tests
-- the IO operation yields a specified result
yields :: IO a -> a -> PropIO
yields act r = PropIO (execIOTest act (return r))

-- two IO operations yield the same result
sameAs :: IO a -> IO a -> PropIO
sameAs a1 a2 = PropIO (execIOTest a1 a2)

-------------------------------------------------------------------------
data Test = Test Result [String] [String]

data Result = Undef | Ok | Falsified [String] | Ambigious [Bool] [String]

type Prop = [Test]

notest :: Test
notest = Test Undef [] []

result :: Test -> Result
result (Test r _ _) = r

setResult :: Result -> Test -> Test
setResult res (Test _ s a) = Test res a s

args, stamp :: Test -> [String]
args  (Test _ a _) = a
stamp (Test _ _ s) = s

updArgs, updStamp :: ([String] -> [String]) -> Test -> Test
updArgs  upd (Test r a s) = Test r (upd a) s
updStamp upd (Test r a s) = Test r a (upd s)

-- Test Specification

test :: a -> ([a] -> Bool) -> Prop
test x f = [setResult res notest]
 where
  xs  = valuesOf x
  res = case valuesOf (f xs) of
          [True]  -> Ok
          [False] -> Falsified (map show xs)
          bs      -> Ambigious bs (map show xs)

--- The property `is x p` is satisfied if `x` has a deterministic value which satisfies `p`.
is :: a -> (a -> Bool) -> Prop
is x f = test x (\xs -> case xs of [y] -> f y; _ -> False)

--- The property `isAlways x p` is satisfied if all values of `x` satisfy `p`.
isAlways :: a -> (a -> Bool) -> Prop
isAlways x  = test x . all

--- The property `isEventually x p` is satisfied if some values of `x` satisfies `p`.
isEventually :: a -> (a -> Bool) -> Prop
isEventually x = test x . any

prop, uniquely, always, eventually :: Bool -> Prop
prop       = uniquely
uniquely   = (`is` id)
always     = (`isAlways` id)
eventually = (`isEventually` id)

failing, successful, deterministic :: _ -> Prop
failing x = test x null
successful x = test x (not . null)
deterministic x = x `is` const True

--- The property `x -=- y` is satisfied if `x` and `y` have deterministic values that are equal.
(-=-) :: a -> a -> Prop
x -=- y = (x,y) `is` uncurry (==)

--- The property `x # n` is satisfied if `x` has `n` values.
(#) :: _ -> Int -> Prop
x # n = test x ((n==) . length . nub)

--- The property `x <~> y` is satisfied if the sets of the values of `x` and `y` are equal.
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

--- The property `x <~~> y` is satisfied if the multisets of the values of `x` and `y` are equal.
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

forAll :: (b -> Prop) -> a -> (a -> b) -> Prop
forAll c x f
  = diagonal [[ updArgs (show y:) t | t <- c (f y) ] | y <- valuesOf x ]

for :: a -> (a -> Prop) -> Prop
for = forAll id

-- Test Annotations

label :: String -> Prop -> Prop
label = map . updStamp . (:)

classify :: Bool -> String -> Prop -> Prop
classify True  name = label name
classify False _    = id

trivial :: Bool -> Prop -> Prop
trivial = (`classify`"trivial")

collect :: a -> Prop -> Prop
collect = label . show

collectAs :: String -> a -> Prop -> Prop
collectAs name = label . ((name++": ")++) . show

-------------------------------------------------------------------------
--- The configuration of property testing.
--- The configuration contains the maximum number of test,
--- the maximum number of condition failures before giving up,
--- an operation the shows the number and arguments of each test,
--- and a status whether it should work quietly.
data Config = Config Int Int (Int -> [String] -> String) Bool

maxTest :: Config -> Int
maxTest (Config n _ _ _) = n

setMaxTest :: Int -> Config -> Config
setMaxTest n (Config _ m f q) = Config n m f q

maxFail :: Config -> Int
maxFail (Config _ n _ _) = n

setMaxFail :: Int -> Config -> Config
setMaxFail m (Config n _ f q) = Config n m f q

every :: Config -> Int -> [String] -> String
every (Config _ _ f _) = f

setEvery :: (Int -> [String] -> String) -> Config -> Config
setEvery f (Config n m _ q) = Config n m f q

isQuiet :: Config -> Bool
isQuiet (Config _ _ _ q) = q

setQuiet :: Bool -> Config -> Config
setQuiet b (Config n m f _) = Config n m f b

--- The default configuration for EasyCheck which some some information
--- for each test.
easyConfig :: Config
--easyConfig = Config 1000 10000
easyConfig = Config 100 10000
                    (\n _ -> let s = ' ':show (n+1) in s ++ [ chr 8 | _ <- s ])
                    False

--- Verbose configuration which shows the arguments of every test.
verboseConfig :: Config
verboseConfig = setEvery (\n xs -> show n ++ ":\n" ++ unlines xs) easyConfig

--- Quiet configuration which shows nothing but failed tests.
quietConfig :: Config
quietConfig = setQuiet True (setEvery (\_ _ -> "") easyConfig)

-------------------------------------------------------------------------
-- Test Functions

suc :: (a -> Prop) -> (b -> a) -> Prop
suc n = forAll n unknown

configCheck0 :: Config -> String -> Prop -> IO Bool
configCheck0 = check

configCheck1 :: Config -> String -> (_ -> Prop) -> IO Bool
configCheck1 config msg = check config msg . suc id

configCheck2 :: Config -> String -> (_ -> _ -> Prop) -> IO Bool
configCheck2 config msg = check config msg . suc (suc id)

configCheck3 :: Config -> String -> (_ -> _ -> _ -> Prop) -> IO Bool
configCheck3 config msg = check config msg . suc (suc (suc id))

configCheck4 :: Config -> String -> (_ -> _ -> _ -> _ -> Prop) -> IO Bool
configCheck4 config msg = check config msg . suc (suc (suc (suc id)))

configCheck5 :: Config -> String -> (_ -> _ -> _ -> _ -> _ -> Prop) -> IO Bool
configCheck5 config msg = check config msg . suc (suc (suc (suc (suc id))))


easyCheck :: String -> Prop -> IO Bool
easyCheck = check easyConfig

easyCheck0 :: String -> Prop -> IO Bool
easyCheck0 = configCheck0 easyConfig

easyCheck1 :: String -> (_ -> Prop) -> IO Bool
easyCheck1 = configCheck1 easyConfig

easyCheck2 :: String -> (_ -> _ -> Prop) -> IO Bool
easyCheck2 = configCheck2 easyConfig

easyCheck3 :: String -> (_ -> _ -> _ -> Prop) -> IO Bool
easyCheck3 = configCheck3 easyConfig

easyCheck4 :: String -> (_ -> _ -> _ -> _ -> Prop) -> IO Bool
easyCheck4 = configCheck4 easyConfig

easyCheck5 :: String -> (_ -> _ -> _ -> _ -> _ -> Prop) -> IO Bool
easyCheck5 = configCheck5 easyConfig

verboseCheck :: String -> Prop -> IO Bool
verboseCheck = check verboseConfig

verboseCheck0 :: String -> Prop -> IO Bool
verboseCheck0 = configCheck0 verboseConfig

verboseCheck1 :: String -> (_ -> Prop) -> IO Bool
verboseCheck1 = configCheck1 verboseConfig

verboseCheck2 :: String -> (_ -> _ -> Prop) -> IO Bool
verboseCheck2 = configCheck2 verboseConfig

verboseCheck3 :: String -> (_ -> _ -> _ -> Prop) -> IO Bool
verboseCheck3 = configCheck3 verboseConfig

verboseCheck4 :: String -> (_ -> _ -> _ -> _ -> Prop) -> IO Bool
verboseCheck4 = configCheck4 verboseConfig

verboseCheck5 :: String -> (_ -> _ -> _ -> _ -> _ -> Prop) -> IO Bool
verboseCheck5 = configCheck5 verboseConfig


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

valuesOf :: a -> [a]
valuesOf
  -- = depthDiag . someSearchTree . (id$##)
  -- = rndDepthDiag 0 . someSearchTree . (id$##)
  -- = levelDiag . someSearchTree . (id$##)
  -- = rndLevelDiag 0 . someSearchTree . (id$##)
   = rndLevelDiagFlat 5 0 . someSearchTree . (id$##)
  -- = allValuesB . someSearchTree . (id$##)

-------------------------------------------------------------------------
-- Internal  operation used by currycheck to check an IO assertion
checkPropIO :: Config -> String -> PropIO -> IO (Maybe String)
checkPropIO config msg p = catchNDIO msg $
  case p of PropIO propio -> propio config msg

execIOTest :: IO a -> IO a -> Config -> String -> IO (Maybe String)
execIOTest act1 act2 config msg =
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

--- Safely executes a property, i.e., catch all exceptions that might occur.
execPropWithMsg :: String -> IO Bool -> IO (Maybe String)
execPropWithMsg msg execprop = catchNDIO msg $ do
  b <- catch execprop
             (\e -> putStrLn (msg ++ ": EXECUTION FAILURE:\n" ++ showError e)
                    >> return False)
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

-------------------------------------------------------------------------
