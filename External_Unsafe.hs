import System.IO.Unsafe (unsafePerformIO)

external_d_C_unsafePerformIO :: Curry_Prelude.C_IO a -> Cover -> ConstStore -> a
external_d_C_unsafePerformIO io _ cs = unsafePerformIO (toIO cs io)
