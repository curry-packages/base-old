--------------------------------------------------------------------------------
--- Test operation for library Data.Char
--------------------------------------------------------------------------------

import Data.Char
import Test.Prop
import Control.SearchTree
import Control.SearchTree.Generators

chrOrdId :: Char -> Prop
chrOrdId c = chr (ord c) -=- c

lowerUpper :: Char -> Prop
lowerUpper c = toLower (toUpper c) -=- toLower c

data Digit = Digit { digit :: Int }
 deriving (Eq,Show)

genDigit :: SearchTree Digit
genDigit = genCons1 Digit (foldr1 (|||) (map genCons0 [0 .. 15]))

digitInt :: Digit -> Prop
digitInt (Digit i) = (i >= 0 && i < 16) ==> digitToInt (intToDigit i) -=- i
