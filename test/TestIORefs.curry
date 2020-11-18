-- Testing operations from library IOExts:

import Data.IORef
import Data.List  ( replace )
import Test.Prop

ioref1 = do
  r <- newIORef (1,True)
  (n,_) <- readIORef r
  writeIORef r (n,False)
  readIORef r

testIORef1 = ioref1 `returns` (1,False)

ioref2 = do
  r <- newIORef [1,2..]
  l1 <- readIORef r
  writeIORef r (replace 42 2 l1)
  l2 <- readIORef r
  return (take 5 l2)

testIORefInfinite = ioref2 `returns` [1,2,42,4,5]


ioref3 = do
  r <- let x free in newIORef (x,x)
  (_,y) <- readIORef r
  doSolve (y=:=1)
  (z,_) <- readIORef r
  return z

testIORefLogic = ioref3 `returns` 1

