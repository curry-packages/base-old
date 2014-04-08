import           System.IO.Unsafe       (unsafePerformIO)
import qualified Curry_Prelude    as CP

external_d_C_unsafePerformIO :: CP.C_IO a -> Cover -> ConstStore -> a
external_d_C_unsafePerformIO io _ cs = unsafePerformIO (toIO io cs)
