------------------------------------------------------------------------------
--- Some tests for library `Data.List`
---
--- To run all tests automatically by CurryCheck, use the command:
---
---     > curry-check TestList
------------------------------------------------------------------------------

import Data.List
import Test.Prop

-- Now we can test properties of our program:

testAppend = ([1,2]++[3,4]) -=- [1,2,3,4]


testNub = (nub [1,3,1,2,3,2,4]) -=- [1,3,2,4]

nubNub :: Eq a => [a] -> [a]
nubNub = nub . nub

propNub = nubNub <=> (nub :: [Int] -> [Int])


testAll = always (all (<5) [1,2,3,4])


-- Specification of `last`:
lastSpec :: Data a => [a] -> a
lastSpec (_ ++ [x]) = x

lastCorrect = last <=> (lastSpec :: [Int] -> Int)


-- Specification of `init` (which is slightly more strict):
initSpec :: Data a => [a] -> [a]
initSpec (xs ++ [_]) = xs

initGroundCorrect :: [Int] -> Prop
initGroundCorrect xs = init xs <~> initSpec xs
