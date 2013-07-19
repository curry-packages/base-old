{-# LANGUAGE MultiParamTypeClasses #-}
import Control.Concurrent
import Control.Monad (when)
import Network
import Network.Socket

import qualified Curry_Prelude as CP

type C_Socket = PrimData Socket

instance ConvertCurryHaskell CP.C_Int PortID where
  toCurry (PortNumber i) = toCurry (toInteger i)
  fromCurry i = PortNumber (fromInteger (fromCurry i))

external_d_C_prim_listenOn :: CP.C_Int -> Cover -> ConstStore -> CP.C_IO C_Socket
external_d_C_prim_listenOn i _ _ = toCurry listenOn i

external_d_C_listenOnFresh :: Cover -> ConstStore -> CP.C_IO (CP.OP_Tuple2 CP.C_Int C_Socket)
external_d_C_listenOnFresh _ _ = toCurry listenOnFreshPort
  where
  listenOnFreshPort :: IO (PortID,Socket)
  listenOnFreshPort = do
    s <- listenOn (PortNumber aNY_PORT)
    p <- Network.socketPort s
    return (p,s)

external_d_C_prim_socketAccept :: C_Socket
  -> Cover -> ConstStore -> CP.C_IO (CP.OP_Tuple2 CP.C_String Curry_IO.C_Handle)
external_d_C_prim_socketAccept socket _ _ =
 toCurry (\s -> Network.accept s >>= \ (h,s,_) -> return (s,OneHandle h)) socket


external_d_C_prim_waitForSocketAccept :: C_Socket -> CP.C_Int
 -> Cover -> ConstStore -> CP.C_IO (CP.C_Maybe (CP.OP_Tuple2 (CP.OP_List CP.C_Char) Curry_IO.C_Handle))
external_d_C_prim_waitForSocketAccept s i _ _ = toCurry wait s i

wait :: Socket -> Int -> IO (Maybe (String, CurryHandle))
wait s t =
  if t < 0
  then Network.accept s >>= \ (h, s, _) -> return (Just (s, OneHandle h))
  else do
    mv <- newEmptyMVar
    tacc <- forkIO (Network.accept s >>= \ (h, s, _) ->
                    putMVar mv (Just (s, OneHandle h)))
    ttim <- forkIO (delay ((fromIntegral t :: Integer) * 1000)
                    >> putMVar mv Nothing)
    res <- takeMVar mv
    maybe (killThread tacc) (\_ -> killThread ttim) res
    return res

-- Like 'threadDelay', but not bounded by an 'Int'
delay :: Integer -> IO ()
delay time = do
  let maxWait = min time $ toInteger (maxBound :: Int)
  threadDelay $ fromInteger maxWait
  when (maxWait /= time) $ delay (time - maxWait)

external_d_C_prim_sClose :: C_Socket -> Cover -> ConstStore -> CP.C_IO CP.OP_Unit
external_d_C_prim_sClose s _ _ = toCurry sClose s

external_d_C_prim_connectToSocket :: CP.C_String -> CP.C_Int
                                  -> Cover -> ConstStore -> CP.C_IO Curry_IO.C_Handle
external_d_C_prim_connectToSocket str i _ _ =
  toCurry (\ s i -> connectTo s i >>= return . OneHandle) str i
