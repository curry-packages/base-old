-------------------------------------------------------------------------
--- EasyCheck is a library for automated, property-based testing of
--- Curry programs. The ideas behind EasyCheck are described in
--- [this paper](http://www-ps.informatik.uni-kiel.de/~sebf/pub/flops08.html)
--- The tool `currycheck` automatically executes tests defined with
--- this library. EasyCheck supports the definition of unit tests
--- (also for I/O operations) and property tests parameterized
--- over some arguments.
---
--- Note that this module defines the interface of EasyCheck to
--- define properties. The operations to actually execute the tests
--- are contained in the accompanying library `Test.EasyCheckExec`.
---
--- @author Sebastian Fischer (with extensions by Michael Hanus)
--- @version April 2016
--- @category general
-------------------------------------------------------------------------

module Test.EasyCheck (

  -- test specification:
  PropIO, returns, sameReturns,

  Config(..), Test, Prop, (==>), for,

  test, is, isAlways, isEventually, uniquely, always, eventually,
  failing, successful, deterministic, (-=-), (#), (<~>), (~>), (<~), (<~~>),
  solutionOf,

  -- test annotations
  label, trivial, classify, collect, collectAs,

  -- configurations
  verboseConfig, quietConfig, easyConfig, setMaxTest, setMaxFail,

  valuesOfSearchTree, valuesOf, Result(..), result,

  -- for EasyCheckExec
  forAllValues, args, stamp, testsOf, ioTestOf

  ) where

import List         ( (\\), delete, diagonal, nub )
import SearchTree   ( SearchTree, someSearchTree )
import SearchTreeTraversal

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

--- Extracts the tests of an I/O property (used by the test runner).
ioTestOf :: PropIO -> (Config -> String -> IO (Maybe String))
ioTestOf (PropIO t) = t

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

-------------------------------------------------------------------------
--- Abstract type to represent a single test for a property to be checked.
data Test = Test Result [String] [String]

--- Data type to represent the result of checking a property.
data Result = Undef | Ok | Falsified [String] | Ambigious [Bool] [String]

--- Abstract type to represent properties to be checked.
data Prop = Prop [Test]

--- Extracts the tests of a property (used by the test runner).
testsOf :: Prop -> [Test]
testsOf (Prop ts) = ts

notest :: Test
notest = Test Undef [] []

--- Extracts the result of a single test.
result :: Test -> Result
result (Test r _ _) = r

setResult :: Result -> Test -> Test
setResult res (Test _ s a) = Test res a s

--- Extracts the arguments of a test.
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
test x f = Prop [setResult res notest]
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
isSameSet xs ys = xs' `subset` ys' && ys' `subset` xs'
 where xs' = nub xs
       ys' = nub ys

isSubsetOf :: [a] -> [a] -> Bool
xs `isSubsetOf` ys = nub xs `subset` ys

subset :: [a] -> [a] -> Bool
xs `subset` ys = null (xs\\ys)

-- compare to lists if they represent the same multi-set
isSameMSet :: [a] -> [a] -> Bool
isSameMSet []     ys = ys == []
isSameMSet (x:xs) ys
  | x `elem` ys  = isSameMSet xs (delete x ys)
  | otherwise    = False

--- A conditional property is tested if the condition evaluates to `True`.
(==>) :: Bool -> Prop -> Prop
cond ==> p =
  if True `elem` valuesOf cond
  then p
  else Prop [notest]

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
 Prop (diagonal [[ updArgs (show y:) t | t <- testsOf (c (f y)) ] | y <- valuesOf x ])

forAllValues :: (b -> Prop) -> [a] -> (a -> b) -> Prop
forAllValues c vals f =
 Prop (diagonal [[ updArgs (show y:) t | t <- testsOf (c (f y)) ] | y <- vals ])

--- The property `for x p` is satisfied if all values `y` of `x`
--- satisfy property `p y`.
for :: a -> (a -> Prop) -> Prop
for x p = forAllValues id (valuesOf x) p

-------------------------------------------------------------------------
-- Test Annotations

label :: String -> Prop -> Prop
label l (Prop ts) = Prop (map (updStamp (l:)) ts)

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
-- Value generation

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
