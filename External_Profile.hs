import System.Mem (performGC)
import System.CPUTime

external_d_C_getProcessInfos :: Cover -> ConstStore ->
   Curry_Prelude.C_IO (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 C_ProcessInfo Curry_Prelude.C_Int))
external_d_C_getProcessInfos _ _ = fromIO $ do
  t <- getCPUTime
  return (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple2 C_RunTime (toCurry (t `div` (10^9))))
                     Curry_Prelude.OP_List)

external_d_C_garbageCollectorOff :: Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
external_d_C_garbageCollectorOff _ _ = toCurry (return () :: IO ()) -- not supported

external_d_C_garbageCollectorOn :: Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
external_d_C_garbageCollectorOn _ _ = toCurry (return () :: IO ()) -- not supported

external_d_C_garbageCollect :: Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
external_d_C_garbageCollect _ _ = toCurry performGC
