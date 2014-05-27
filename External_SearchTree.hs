import Control.Applicative
import Control.Monad
import MonadSearch
import GHC.Exts (Int (I#))

instance Functor C_SearchTree where
  fmap = liftM

instance Applicative C_SearchTree where
  pure = return
  (<*>) = ap

instance Monad C_SearchTree where
  return = C_Value

  C_Fail    d >>= _ = C_Fail d
  C_Value   x >>= f = f x
  C_Or    x y >>= f = C_Or (x >>= f) (y >>= f)

  Choice_C_SearchTree  cd i x y >>= f
    = Choice_C_SearchTree  cd i  (x >>= f) (y >>= f)
  Choices_C_SearchTree cd i xs  >>= f
    = Choices_C_SearchTree cd i  (map (>>= f) xs)
  Guard_C_SearchTree   cd cs x  >>= f = Guard_C_SearchTree   cd cs (x >>= f)
  Fail_C_SearchTree    cd info  >>= _ = Fail_C_SearchTree    cd info

instance Alternative C_SearchTree where
  (<|>) = mplus
  empty = mzero

instance MonadPlus C_SearchTree where
  mzero = C_Fail (Curry_Prelude.C_Int -1#)
  mplus = C_Or

instance MonadSearch C_SearchTree where
  splus            = Choice_C_SearchTree
  ssum             = Choices_C_SearchTree
  szero (I# d) _   = C_Fail (Curry_Prelude.C_Int d)
  constrainMSearch = Guard_C_SearchTree

external_d_C_someSearchTree :: NormalForm a
                            => a -> Cover -> ConstStore -> C_SearchTree a
external_d_C_someSearchTree = encapsulatedSearch
