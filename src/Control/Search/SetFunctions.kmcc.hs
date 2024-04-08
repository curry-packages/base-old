{-# LANGUAGE MagicHash, TypeOperators #-}
import BasicDefinitions
import MemoizedCurry
import Prelude
import Control.Monad.State

searchdotSetFunctionsdotset0_Det# :: a
searchdotSetFunctionsdotset0_Det# = undefined

searchdotSetFunctionsdotset0_ND# :: Curryable a => Curry (a :-> Values_ND a)
searchdotSetFunctionsdotset0_ND# = returnFunc set0

searchdotSetFunctionsdotset1_Det# :: a
searchdotSetFunctionsdotset1_Det# = undefined

searchdotSetFunctionsdotset1_ND# :: (Curryable a, Curryable b) => Curry ((a :-> b) :-> a :-> Values_ND b)
searchdotSetFunctionsdotset1_ND# = returnFunc $ \f -> returnFunc $ \arg1 -> set1 f arg1

searchdotSetFunctionsdotset2_Det# :: a
searchdotSetFunctionsdotset2_Det# = undefined

searchdotSetFunctionsdotset2_ND# :: (Curryable a, Curryable b, Curryable c) => Curry ((a :-> b :-> c) :-> a :-> b :-> Values_ND c)
searchdotSetFunctionsdotset2_ND# = returnFunc $ \f -> returnFunc $ \arg1 -> returnFunc $ \arg2 -> set2 f arg1 arg2

searchdotSetFunctionsdotset3_Det# :: a
searchdotSetFunctionsdotset3_Det# = undefined

searchdotSetFunctionsdotset3_ND# :: (Curryable a, Curryable b, Curryable c, Curryable d) => Curry ((a :-> b :-> c :-> d) :-> a :-> b :-> c :-> Values_ND d)
searchdotSetFunctionsdotset3_ND# = returnFunc $ \f -> returnFunc $ \arg1 -> returnFunc $ \arg2 -> returnFunc $ \arg3 -> set3 f arg1 arg2 arg3

searchdotSetFunctionsdotset4_Det# :: a
searchdotSetFunctionsdotset4_Det# = undefined

searchdotSetFunctionsdotset4_ND# :: (Curryable a, Curryable b, Curryable c, Curryable d, Curryable e) => Curry ((a :-> b :-> c :-> d :-> e) :-> a :-> b :-> c :-> d :-> Values_ND e)
searchdotSetFunctionsdotset4_ND# = returnFunc $ \f -> returnFunc $ \arg1 -> returnFunc $ \arg2 -> returnFunc $ \arg3 -> returnFunc $ \arg4 -> set4 f arg1 arg2 arg3 arg4

searchdotSetFunctionsdotset5_Det# :: a
searchdotSetFunctionsdotset5_Det# = undefined

searchdotSetFunctionsdotset5_ND# :: (Curryable a, Curryable b, Curryable c, Curryable d, Curryable e, Curryable f) => Curry ((a :-> b :-> c :-> d :-> e :-> f) :-> a :-> b :-> c :-> d :-> e :-> Values_ND f)
searchdotSetFunctionsdotset5_ND# = returnFunc $ \f -> returnFunc $ \arg1 -> returnFunc $ \arg2 -> returnFunc $ \arg3 -> returnFunc $ \arg4 -> returnFunc $ \arg5 -> set5 f arg1 arg2 arg3 arg4 arg5

searchdotSetFunctionsdotset6_Det# :: a
searchdotSetFunctionsdotset6_Det# = undefined

searchdotSetFunctionsdotset6_ND# :: (Curryable a, Curryable b, Curryable c, Curryable d, Curryable e, Curryable f, Curryable g) => Curry ((a :-> b :-> c :-> d :-> e :-> f :-> g) :-> a :-> b :-> c :-> d :-> e :-> f :-> Values_ND g)
searchdotSetFunctionsdotset6_ND# = returnFunc $ \f -> returnFunc $ \arg1 -> returnFunc $ \arg2 -> returnFunc $ \arg3 -> returnFunc $ \arg4 -> returnFunc $ \arg5 -> returnFunc $ \arg6 -> set6 f arg1 arg2 arg3 arg4 arg5 arg6

searchdotSetFunctionsdotset7_Det# :: a
searchdotSetFunctionsdotset7_Det# = undefined

searchdotSetFunctionsdotset7_ND# :: (Curryable a, Curryable b, Curryable c, Curryable d, Curryable e, Curryable f, Curryable g, Curryable h) => Curry ((a :-> b :-> c :-> d :-> e :-> f :-> g :-> h) :-> a :-> b :-> c :-> d :-> e :-> f :-> g :-> Values_ND h)
searchdotSetFunctionsdotset7_ND# = returnFunc $ \f -> returnFunc $ \arg1 -> returnFunc $ \arg2 -> returnFunc $ \arg3 -> returnFunc $ \arg4 -> returnFunc $ \arg5 -> returnFunc $ \arg6 -> returnFunc $ \arg7 -> set7 f arg1 arg2 arg3 arg4 arg5 arg6 arg7


-- Convert a tree list to a curry list.
transListTree :: ListTree a -> CList_ND a
transListTree NilTree         = CList_ND
transListTree (ConsTree x xs) =
  CCons_ND (treeToCurry x) (treeToCurry (fmap transListTree xs))


set0 :: Curryable a
     => Curry a -> Curry (Values_ND a)
set0 f = do
  lvl <- currentLevel <$> get
  Values_ND . return . transListTree <$> captureWithLvl lvl (groundNormalForm f)

set1 :: (Curryable a, Curryable b)
     => Curry (a :-> b) -> Curry a -> Curry (Values_ND b)
set1 f arg = do
  lvl <- currentLevel <$> get
  Values_ND . return . transListTree <$> captureWithLvl lvl (groundNormalForm (f
    `app` (setLevelC (succ lvl) arg)))

set2 :: (Curryable a, Curryable b, Curryable c)
     => Curry (a :-> b :-> c) -> Curry a -> Curry b -> Curry (Values_ND c)
set2 f arg1 arg2 = do
  lvl <- currentLevel <$> get
  Values_ND . return . transListTree <$> captureWithLvl lvl (groundNormalForm (f
    `app` (setLevelC (succ lvl) arg1)
    `app` (setLevelC (succ lvl) arg2)))

set3 :: (Curryable a, Curryable b, Curryable c, Curryable d)
     => Curry (a :-> b :-> c :-> d) -> Curry a -> Curry b -> Curry c -> Curry (Values_ND d)
set3 f arg1 arg2 arg3 = do
  lvl <- currentLevel <$> get
  Values_ND . return . transListTree <$> captureWithLvl lvl (groundNormalForm (f
    `app` (setLevelC (succ lvl) arg1)
    `app` (setLevelC (succ lvl) arg2)
    `app` (setLevelC (succ lvl) arg3)))

set4 :: (Curryable a, Curryable b, Curryable c, Curryable d, Curryable e)
     => Curry (a :-> b :-> c :-> d :-> e) -> Curry a -> Curry b -> Curry c -> Curry d -> Curry (Values_ND e)
set4 f arg1 arg2 arg3 arg4 = do
  lvl <- currentLevel <$> get
  Values_ND . return . transListTree <$> captureWithLvl lvl (groundNormalForm (f
    `app` (setLevelC (succ lvl) arg1)
    `app` (setLevelC (succ lvl) arg2)
    `app` (setLevelC (succ lvl) arg3)
    `app` (setLevelC (succ lvl) arg4)))

set5 :: (Curryable a, Curryable b, Curryable c, Curryable d, Curryable e, Curryable f)
     => Curry (a :-> b :-> c :-> d :-> e :-> f) -> Curry a -> Curry b -> Curry c -> Curry d -> Curry e -> Curry (Values_ND f)
set5 f arg1 arg2 arg3 arg4 arg5 = do
  lvl <- currentLevel <$> get
  Values_ND . return . transListTree <$> captureWithLvl lvl (groundNormalForm (f
    `app` (setLevelC (succ lvl) arg1)
    `app` (setLevelC (succ lvl) arg2)
    `app` (setLevelC (succ lvl) arg3)
    `app` (setLevelC (succ lvl) arg4)
    `app` (setLevelC (succ lvl) arg5)))

set6 :: (Curryable a, Curryable b, Curryable c, Curryable d, Curryable e, Curryable f, Curryable g)
     => Curry (a :-> b :-> c :-> d :-> e :-> f :-> g) -> Curry a -> Curry b -> Curry c -> Curry d -> Curry e -> Curry f -> Curry (Values_ND g)
set6 f arg1 arg2 arg3 arg4 arg5 arg6 = do
  lvl <- currentLevel <$> get
  Values_ND . return . transListTree <$> captureWithLvl lvl (groundNormalForm (f
    `app` (setLevelC (succ lvl) arg1)
    `app` (setLevelC (succ lvl) arg2)
    `app` (setLevelC (succ lvl) arg3)
    `app` (setLevelC (succ lvl) arg4)
    `app` (setLevelC (succ lvl) arg5)
    `app` (setLevelC (succ lvl) arg6)))

set7 :: (Curryable a, Curryable b, Curryable c, Curryable d, Curryable e, Curryable f, Curryable g, Curryable h)
     => Curry (a :-> b :-> c :-> d :-> e :-> f :-> g :-> h) -> Curry a -> Curry b -> Curry c -> Curry d -> Curry e -> Curry f -> Curry g -> Curry (Values_ND h)
set7 f arg1 arg2 arg3 arg4 arg5 arg6 arg7 = do
  lvl <- currentLevel <$> get
  Values_ND . return . transListTree <$> captureWithLvl lvl (groundNormalForm (f
    `app` (setLevelC (succ lvl) arg1)
    `app` (setLevelC (succ lvl) arg2)
    `app` (setLevelC (succ lvl) arg3)
    `app` (setLevelC (succ lvl) arg4)
    `app` (setLevelC (succ lvl) arg5)
    `app` (setLevelC (succ lvl) arg6)
    `app` (setLevelC (succ lvl) arg7)))
