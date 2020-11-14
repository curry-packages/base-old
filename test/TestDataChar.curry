--------------------------------------------------------------------------------
--- Test operation for library Data.Char
--------------------------------------------------------------------------------

import Data.Char
import Test.Prop

chrOrdId :: Char -> Prop
chrOrdId c = chr (ord c) -=- c

lowerUpper :: Char -> Prop
lowerUpper c = toLower (toUpper c) -=- toLower c

digitInt :: Int -> Prop
digitInt i = (i >= 0 && i < 16) ==> digitToInt (intToDigit i) -=- i
