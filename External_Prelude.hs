{-# LANGUAGE BangPatterns, CPP, MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses, ScopedTypeVariables #-}

import qualified Control.Exception as C

-- ATTENTION: Do not introduce line breaks in import declarations as these
-- are not recognized!
import GHC.Exts (Int (I#), Int#, (==#), (/=#), (<#), (>#), (<=#))
import GHC.Exts ((+#), (-#), (*#), quotInt#, remInt#, negateInt#)
import GHC.Exts (Float (F#), Float#, eqFloat#, leFloat#, negateFloat#)
import GHC.Exts (Char (C#), Char#, eqChar#, leChar#, ord#, chr#)
import System.IO

import CurryException
import Debug          (internalError)
import FailInfo       (customFail)
import PrimTypes

#if __GLASGOW_HASKELL__ > 706
import GHC.Exts (isTrue#)
#endif

-- #endimport - do not remove this line!

#if !(__GLASGOW_HASKELL__ > 706)
isTrue# :: Bool -> Bool
{-# INLINE isTrue# #-}
isTrue# x = x
#endif

-- ---------------------------------------------------------------------------
-- Curry types
-- ---------------------------------------------------------------------------

-- Class for Curry types
class (Show a, Read a, NonDet a, Generable a, NormalForm a, Unifiable a)
      => Curry a where
  -- implementation of strict equalit (==) for a data type
  (=?=) :: a -> a -> Cover -> ConstStore -> C_Bool
  (=?=) = error "(==) is undefined"
  -- implementation of less-or-equal (<=) for a data type
  (<?=) :: a -> a -> Cover -> ConstStore -> C_Bool
  (<?=) = error "(<=) is undefined"

instance Curry (PrimData a) where
  (=?=) = error "(==) is undefined for primitive data"
  (<?=) = error "(<=) is undefined for primitive data"

-- BEGIN GENERATED FROM PrimTypes.curry
instance Curry C_Success where
  (=?=) (Choice_C_Success cd i x y) z d cs = narrow cd i (((x =?= z) d) cs) (((y =?= z) d) cs)
  (=?=) (Choices_C_Success cd i xs) y d cs = narrows cs cd i (\x -> ((x =?= y) d) cs) xs
  (=?=) (Guard_C_Success cd c e) y d cs = guardCons cd c (((e =?= y) d) (addCs c cs))
  (=?=) (Fail_C_Success cd info) _ _ _ = failCons cd info
  (=?=) z (Choice_C_Success cd i x y) d cs = narrow cd i (((z =?= x) d) cs) (((z =?= y) d) cs)
  (=?=) y (Choices_C_Success cd i xs) d cs = narrows cs cd i (\x -> ((y =?= x) d) cs) xs
  (=?=) y (Guard_C_Success cd c e) d cs = guardCons cd c (((y =?= e) d) (addCs c cs))
  (=?=) _ (Fail_C_Success cd info) _ _ = failCons cd info
  (=?=) C_Success C_Success d cs = C_True
  (<?=) (Choice_C_Success cd i x y) z d cs = narrow cd i (((x <?= z) d) cs) (((y <?= z) d) cs)
  (<?=) (Choices_C_Success cd i xs) y d cs = narrows cs cd i (\x -> ((x <?= y) d) cs) xs
  (<?=) (Guard_C_Success cd c e) y d cs = guardCons cd c (((e <?= y) d) (addCs c cs))
  (<?=) (Fail_C_Success cd info) _ _ _ = failCons cd info
  (<?=) z (Choice_C_Success cd i x y) d cs = narrow cd i (((z <?= x) d) cs) (((z <?= y) d) cs)
  (<?=) y (Choices_C_Success cd i xs) d cs = narrows cs cd i (\x -> ((y <?= x) d) cs) xs
  (<?=) y (Guard_C_Success cd c e) d cs = guardCons cd c (((y <?= e) d) (addCs c cs))
  (<?=) _ (Fail_C_Success cd info) _ _ = failCons cd info
  (<?=) C_Success C_Success d cs = C_True
-- END GENERATED FROM PrimTypes.curry

instance (Curry t0, Curry t1) => Curry (Func t0 t1) where
  (=?=) = error "(==) is undefined for functions"
  (<?=) = error "(<=) is undefined for functions"

instance Curry t0 => Curry (C_IO t0) where
  (=?=) = error "(==) is undefined for IO actions"
  (<?=) = error "(<=) is undefined for IO actions"

instance NonDet b => Curry (a -> b) where
  (=?=) = error "(==) is undefined for functions"
  (<?=) = error "(<=) is undefined for functions"

-- -----------------------------------------------------------------------------
-- Int representation
-- -----------------------------------------------------------------------------

-- BEGIN GENERATED FROM PrimTypes.curry
data C_Int
     = C_Int Int#
     | C_CurryInt BinInt
     | Choice_C_Int Cover ID C_Int C_Int
     | Choices_C_Int Cover ID ([C_Int])
     | Fail_C_Int Cover FailInfo
     | Guard_C_Int Cover Constraints C_Int

instance Show C_Int where
  showsPrec d (Choice_C_Int cd i x y) = showsChoice d cd i x y
  showsPrec d (Choices_C_Int cd i xs) = showsChoices d cd i xs
  showsPrec d (Guard_C_Int cd c e) = showsGuard d cd c e
  showsPrec _ (Fail_C_Int _ _) = showChar '!'
  showsPrec d (C_Int x1) = shows (I# x1)
  showsPrec d (C_CurryInt x1) = case ((\x _ _ -> x) $## x1) (error "Show C_Int: nesting depth used") emptyCs of
    Choice_BinInt _ _ _ _ -> shows x1
    Choices_BinInt _ _ _  -> shows x1
    Fail_BinInt _ _       -> shows x1
    Guard_BinInt _ _ _    -> shows x1
    gnfBinInt             -> shows (I# (curryint2primint gnfBinInt))

instance Read C_Int where
  readsPrec d s = map readInt (readsPrec d s) where readInt (I# i, s) = (C_Int i, s)

instance NonDet C_Int where
  choiceCons = Choice_C_Int
  choicesCons = Choices_C_Int
  failCons = Fail_C_Int
  guardCons = Guard_C_Int
  try (Choice_C_Int cd i x y) = tryChoice cd i x y
  try (Choices_C_Int cd i xs) = tryChoices cd i xs
  try (Fail_C_Int cd info) = Fail cd info
  try (Guard_C_Int cd c e) = Guard cd c e
  try x = Val x
  match f _ _ _ _ _ (Choice_C_Int cd i x y) = f cd i x y
  match _ f _ _ _ _ (Choices_C_Int cd i@(NarrowedID _ _) xs) = f cd i xs
  match _ _ f _ _ _ (Choices_C_Int cd i@(FreeID _ _) xs) = f cd i xs
  match _ _ _ _ _ _ (Choices_C_Int _  i _) = error ("Prelude.Int.match: Choices with ChoiceID " ++ (show i))
  match _ _ _ f _ _ (Fail_C_Int cd info) = f cd info
  match _ _ _ _ f _ (Guard_C_Int cd cs e) = f cd cs e
  match _ _ _ _ _ f x = f x

instance Generable C_Int where
  generate s cd = Choices_C_Int cd (freeID [1] s) [C_CurryInt (generate (leftSupply s) cd)]

instance NormalForm C_Int where
  ($!!) cont x@(C_Int _) cd cs = cont x cd cs
  ($!!) cont (C_CurryInt x1) cd cs = ((\y1 -> cont (C_CurryInt y1)) $!! x1) cd cs
  ($!!) cont (Choice_C_Int d i x y) cd cs = nfChoice cont d i x y cd cs
  ($!!) cont (Choices_C_Int d i xs) cd cs = nfChoices cont d i xs cd cs
  ($!!) cont (Guard_C_Int d c x) cd cs = guardCons d c ((cont $!! x) cd $! (addCs c cs))
  ($!!) _ (Fail_C_Int cd info) _ _ = failCons cd info
  ($##) cont x@(C_Int _) cd cs = cont x cd cs
  ($##) cont (C_CurryInt x1) cd cs = ((\y1 -> cont (C_CurryInt y1)) $## x1) cd cs
  ($##) cont (Choice_C_Int d i x y) cd cs = gnfChoice cont d i x y cd cs
  ($##) cont (Choices_C_Int d i xs) cd cs = gnfChoices cont d i xs cd cs
  ($##) cont (Guard_C_Int d c x) cd cs = guardCons d c ((cont $## x) cd $! (addCs c cs))
  ($##) _ (Fail_C_Int d info) _ _ = failCons d info
  searchNF search cont x@(C_Int _) = cont x
  searchNF search cont (C_CurryInt x1) = search (\y1 -> cont (C_CurryInt y1)) x1
  searchNF _ _ x = error ("Prelude.Int.searchNF: no constructor: " ++ (show x))

instance Unifiable C_Int where
  (=.=) (C_Int      x1) (C_Int      y1) cd _  = if isTrue# (x1 ==# y1) then C_Success else Fail_C_Success cd defFailInfo
  (=.=) (C_Int      x1) (C_CurryInt y1) cd cs = ((primint2curryint x1) =:= y1) cd cs
  (=.=) (C_CurryInt x1) (C_Int      y1) cd cs = (x1 =:= (primint2curryint y1)) cd cs
  (=.=) (C_CurryInt x1) (C_CurryInt y1) cd cs = (x1 =:= y1) cd cs
  (=.=) _               _               cd _  = Fail_C_Success cd defFailInfo
  (=.<=) (C_Int      x1) (C_Int      y1) cd _ = if isTrue# (x1 ==# y1) then C_Success else Fail_C_Success cd defFailInfo
  (=.<=) (C_Int      x1) (C_CurryInt y1) cd cs = ((primint2curryint x1) =:<= y1) cd cs
  (=.<=) (C_CurryInt x1) (C_Int      y1) cd cs = (x1 =:<= (primint2curryint y1)) cd cs
  (=.<=) (C_CurryInt x1) (C_CurryInt y1) cd cs = (x1 =:<= y1) cd cs
  (=.<=) _ _ cd _= Fail_C_Success cd defFailInfo
  bind cd i (C_Int      x2) = (i :=: ChooseN 0 1) : bind cd (leftID i) (primint2curryint x2)
  bind cd i (C_CurryInt x2) = (i :=: ChooseN 0 1) : bind cd (leftID i) x2
  bind cd i (Choice_C_Int d j l r) = [(ConstraintChoice d j (bind cd i l) (bind cd i r))]
  bind cd i (Choices_C_Int d j@(FreeID _ _) xs) = bindOrNarrow cd i d j xs
  bind cd i (Choices_C_Int d j@(NarrowedID _ _) xs) = [(ConstraintChoices d j (map (bind cd i) xs))]
  bind _  _ c@(Choices_C_Int _ i@(ChoiceID _) _) = error ("Prelude.Int.bind: Choices with ChoiceID: " ++ (show c))
  bind _ _ (Fail_C_Int _ info) = [Unsolvable info]
  bind cd  i (Guard_C_Int _ cs e) = getConstrList cs ++ (bind cd i e)
  lazyBind cd i (C_Int      x2) = [i :=: ChooseN 0 1, leftID i :=: LazyBind (lazyBind cd (leftID i) (primint2curryint x2))]
  lazyBind cd i (C_CurryInt x2) = [i :=: ChooseN 0 1, leftID i :=: LazyBind (lazyBind cd (leftID i) x2)]
  lazyBind cd i (Choice_C_Int d j l r) = [(ConstraintChoice d j (lazyBind cd i l) (lazyBind cd i r))]
  lazyBind cd i (Choices_C_Int d j@(FreeID _ _) xs) = lazyBindOrNarrow cd i d j xs
  lazyBind cd i (Choices_C_Int d j@(NarrowedID _ _) xs) = [(ConstraintChoices d j (map (lazyBind cd i) xs))]
  lazyBind _  _ c@(Choices_C_Int _ i@(ChoiceID _) _) = error ("Prelude.Int.lazyBind: Choices with ChoiceID: " ++ (show c))
  lazyBind _  _ (Fail_C_Int _ info) = [Unsolvable info]
  lazyBind cd i (Guard_C_Int _ cs e) = getConstrList cs ++ [(i :=: (LazyBind (lazyBind cd i e)))]

instance Curry C_Int where
  (=?=) (Choice_C_Int d i x y) z cd cs = narrow d i ((x =?= z) cd cs) ((y =?= z) cd cs)
  (=?=) (Choices_C_Int d i xs) y cd cs = narrows cs d i (\x -> (x =?= y) cd cs) xs
  (=?=) (Guard_C_Int d c x) y cd cs = guardCons d c ((x =?= y) cd $! (addCs c cs))
  (=?=) (Fail_C_Int d info) _ _ _ = failCons d info
  (=?=) z (Choice_C_Int d i x y) cd cs = narrow d i ((z =?= x) cd cs) ((z =?= y) cd cs)
  (=?=) y (Choices_C_Int d i xs) cd cs = narrows cs d i (\x -> (y =?= x) cd cs) xs
  (=?=) y (Guard_C_Int d c x) cd cs = guardCons d c ((y =?= x) cd $! (addCs c cs))
  (=?=) _ (Fail_C_Int d info) _ _ = failCons d info
  (=?=) (C_Int      x1) (C_Int      y1) _ _ = toCurry (isTrue# (x1 ==# y1))
  (=?=) (C_Int      x1) (C_CurryInt y1) cd cs = ((primint2curryint x1) =?= y1) cd cs
  (=?=) (C_CurryInt x1) (C_Int      y1) cd cs = (x1 =?= (primint2curryint y1)) cd cs
  (=?=) (C_CurryInt x1) (C_CurryInt y1) cd cs = (x1 =?= y1) cd cs
  (<?=) (Choice_C_Int d i x y) z cd cs = narrow d i ((x <?= z) cd cs) ((y <?= z) cd cs)
  (<?=) (Choices_C_Int d i xs) y cd cs = narrows cs d i (\x -> (x<?= y) cd cs) xs
  (<?=) (Guard_C_Int d c x) y cd cs = guardCons d c ((x <?= y) cd $! (addCs c cs))
  (<?=) (Fail_C_Int d info) _ _ _ = failCons d info
  (<?=) z (Choice_C_Int d i x y) cd cs = narrow d i ((z <?= x) cd cs) ((z <?= y) cd cs)
  (<?=) y (Choices_C_Int d i xs) cd cs = narrows cs d i (\x -> (y <?= x) cd cs) xs
  (<?=) y (Guard_C_Int d c x) cd cs = guardCons d c ((y <?= x) cd $! (addCs c cs))
  (<?=) _ (Fail_C_Int d info) _ _ = failCons d info
  (<?=) (C_Int      x1) (C_Int      y1) _ _ = toCurry (isTrue# (x1 <=# y1))
  (<?=) (C_Int      x1) (C_CurryInt y1) cd cs = ((primint2curryint x1) `d_C_lteqInteger` y1) cd cs
  (<?=) (C_CurryInt x1) (C_Int      y1) cd cs = (x1 `d_C_lteqInteger` (primint2curryint y1)) cd cs
  (<?=) (C_CurryInt x1) (C_CurryInt y1) cd cs = (x1 `d_C_lteqInteger` y1) cd cs
-- END GENERATED FROM PrimTypes.curry

primint2curryint :: Int# -> BinInt
primint2curryint n
  | isTrue# (n <#  0#) = Neg (primint2currynat (negateInt# n))
  | isTrue# (n ==# 0#) = Zero
  | otherwise          = Pos (primint2currynat n)

primint2currynat :: Int# -> Nat
primint2currynat n
  | isTrue# (n ==# 1#)                = IHi
  | isTrue# ((n `remInt#` 2#) ==# 0#) = O (primint2currynat (n `quotInt#` 2#))
  | otherwise                         = I (primint2currynat (n `quotInt#` 2#))

curryint2primint :: BinInt -> Int#
curryint2primint Zero    = 0#
curryint2primint (Pos n) = currynat2primint n
curryint2primint (Neg n) = negateInt# (currynat2primint n)
curryint2primint int     = error ("KiCS2 error: Prelude.curryint2primint: no ground term, but " ++ show int)

currynat2primint :: Nat -> Int#
currynat2primint IHi   = 1#
currynat2primint (O n) = 2# *# currynat2primint n
currynat2primint (I n) = 2# *# currynat2primint n +# 1#
currynat2primint nat   = error ("KiCS2 error: Prelude.currynat2primint: no ground term, but " ++ show nat)

-- -----------------------------------------------------------------------------
-- Float representation
-- -----------------------------------------------------------------------------

data C_Float
     = C_Float Float#
     | Choice_C_Float Cover ID C_Float C_Float
     | Choices_C_Float Cover ID ([C_Float])
     | Fail_C_Float Cover FailInfo
     | Guard_C_Float Cover (Constraints) C_Float

instance Show C_Float where
  showsPrec d (Choice_C_Float cd i x y) = showsChoice d cd i x y
  showsPrec d (Choices_C_Float cd i xs) = showsChoices d cd i xs
  showsPrec d (Guard_C_Float cd c e) = showsGuard d cd c e
  showsPrec d (Fail_C_Float _ _) = showChar '!'
  showsPrec d (C_Float x1) = shows (F# x1)

instance Read C_Float where
  readsPrec d s = map readFloat (readsPrec d s) where readFloat (F# f, s) = (C_Float f, s)

instance NonDet C_Float where
  choiceCons = Choice_C_Float
  choicesCons = Choices_C_Float
  failCons = Fail_C_Float
  guardCons = Guard_C_Float
  try (Choice_C_Float cd i x y) = tryChoice cd i x y
  try (Choices_C_Float cd i xs) = tryChoices cd i xs
  try (Fail_C_Float cd info) = Fail cd info
  try (Guard_C_Float cd c e) = Guard cd c e
  try x = Val x
  match f _ _ _ _ _ (Choice_C_Float cd i x y) = f cd i x y
  match _ f _ _ _ _ (Choices_C_Float cd i@(NarrowedID _ _) xs) = f cd i xs
  match _ _ f _ _ _ (Choices_C_Float cd i@(FreeID _ _) xs) = f cd i xs
  match _ _ _ _ _ _ (Choices_C_Float cd i@(ChoiceID _) _) = error ("Prelude.Float.match: Choices with ChoiceID " ++ (show i))
  match _ _ _ f _ _ (Fail_C_Float cd info) = f cd info
  match _ _ _ _ f _ (Guard_C_Float cd cs e) = f cd cs e
  match _ _ _ _ _ f x = f x

instance Generable C_Float where
  generate = error "No generator for C_Float"

instance NormalForm C_Float where
  ($!!) cont x@(C_Float _) cd cs = cont x cd cs
  ($!!) cont (Choice_C_Float d i x y) cd cs = nfChoice cont d i x y cd cs
  ($!!) cont (Choices_C_Float d i xs) cd cs = nfChoices cont d i xs cd cs
  ($!!) cont (Guard_C_Float d c x) cd cs = guardCons d c ((cont $!! x) cd $! (addCs c cs))
  ($!!) _ (Fail_C_Float d info) _ _ = failCons d info
  ($##) cont x@(C_Float _) cd cs = cont x cd cs
  ($##) cont (Choice_C_Float d i x y) cd cs = gnfChoice cont d i x y cd cs
  ($##) cont (Choices_C_Float d i xs) cd cs = gnfChoices cont d i xs cd cs
  ($##) cont (Guard_C_Float d c x) cd cs = guardCons d c ((cont $## x) cd $! (addCs c cs))
  ($##) _ (Fail_C_Float d info) _ _ = failCons d info
  searchNF search cont x@(C_Float _) = cont x
  searchNF _ _ x = error ("Prelude.Float.searchNF: no constructor: " ++ (show x))

instance Unifiable C_Float where
  (=.=) (C_Float x1) (C_Float y1) cd _  = if isTrue# (x1 `eqFloat#` y1) then C_Success else Fail_C_Success cd defFailInfo
  (=.<=) (C_Float x1) (C_Float y1) cd _  = if isTrue# (x1 `eqFloat#` y1) then C_Success else Fail_C_Success cd defFailInfo
  bind cd i (Choice_C_Float d j l r) = [(ConstraintChoice d j (bind cd i l) (bind cd i r))]
  bind cd i (Choices_C_Float d j@(FreeID _ _) xs) = bindOrNarrow cd i d j xs
  bind cd i (Choices_C_Float d j@(NarrowedID _ _) xs) = [(ConstraintChoices d j (map (bind cd i) xs))]
  bind _  _ c@(Choices_C_Float _ i _) = error ("Prelude.Float.bind: Choices with ChoiceID: " ++ (show c))
  bind _  _ (Fail_C_Float _ info) = [Unsolvable info]
  bind cd i (Guard_C_Float _ cs e) = getConstrList cs ++ (bind cd i e)
  lazyBind cd i (Choice_C_Float d j l r) = [(ConstraintChoice d j (lazyBind cd i l) (lazyBind cd i r))]
  lazyBind cd i (Choices_C_Float d j@(FreeID _ _) xs) = lazyBindOrNarrow cd i d j xs
  lazyBind cd i (Choices_C_Float d j@(NarrowedID _ _) xs) = [(ConstraintChoices d j (map (lazyBind cd i) xs))]
  lazyBind _  _ c@(Choices_C_Float _ i _) = error ("Prelude.Float.lazyBind: Choices with ChoiceID: " ++ (show c))
  lazyBind _  _ (Fail_C_Float _ info) = [Unsolvable info]
  lazyBind cd  i (Guard_C_Float _ cs e) = getConstrList cs ++ [(i :=: (LazyBind (lazyBind cd i e)))]

instance Curry C_Float where
  (=?=) (Choice_C_Float d i x y) z cd cs = narrow d i ((x =?= z) cd cs) ((y =?= z) cd cs)
  (=?=) (Choices_C_Float d i xs) y cd cs = narrows cs d i (\x -> (x =?= y) cd cs) xs
  (=?=) (Guard_C_Float d c x) y cd cs = guardCons d c ((x =?= y) cd  $! (addCs c cs))
  (=?=) (Fail_C_Float d info) _ _ _= failCons d info
  (=?=) z (Choice_C_Float d i x y) cd cs = narrow d i ((z =?= x) cd cs) ((z =?= y) cd cs)
  (=?=) y (Choices_C_Float d i xs) cd cs = narrows cs d i (\x -> (y =?= x) cd cs) xs
  (=?=) y (Guard_C_Float d c x) cd cs = guardCons d c ((y =?= x) cd  $! (addCs c cs))
  (=?=) _ (Fail_C_Float d info) _ _ = failCons d info
  (=?=) (C_Float x1) (C_Float y1) _ _ = toCurry (isTrue# (x1 `eqFloat#` y1))
  (<?=) (Choice_C_Float d i x y) z cd cs = narrow d i ((x <?= z) cd cs) ((y <?= z) cd cs)
  (<?=) (Choices_C_Float d i xs) y cd cs = narrows cs d i (\x -> (x <?= y) cd cs) xs
  (<?=) (Guard_C_Float d c x) y cd cs = guardCons d c ((x <?= y) cd $! (addCs c cs))
  (<?=) (Fail_C_Float d info) _ _ _ = failCons d info
  (<?=) z (Choice_C_Float d i x y) cd cs = narrow d i ((z <?= x) cd cs) ((z <?= y) cd cs)
  (<?=) y (Choices_C_Float d i xs) cd cs = narrows cs d i (\x -> (y <?= x) cd cs) xs
  (<?=) y (Guard_C_Float d c x) cd cs = guardCons d c ((y <?= x) cd $! (addCs c cs))
  (<?=) _ (Fail_C_Float d info) _ _ = failCons d info
  (<?=) (C_Float x1) (C_Float y1) _ _ = toCurry (isTrue# (x1 `leFloat#` y1))

-- ---------------------------------------------------------------------------
-- Char
-- ---------------------------------------------------------------------------

data C_Char
     = C_Char Char#
     | CurryChar BinInt
     | Choice_C_Char Cover ID C_Char C_Char
     | Choices_C_Char Cover ID ([C_Char])
     | Fail_C_Char Cover FailInfo
     | Guard_C_Char Cover (Constraints) C_Char

instance Show C_Char where
  showsPrec d (Choice_C_Char cd i x y) = showsChoice d cd i x y
  showsPrec d (Choices_C_Char cd i xs) = showsChoices d cd i xs
  showsPrec d (Guard_C_Char cd c e) = showsGuard d d c e
  showsPrec d (Fail_C_Char _ _) = showChar '!'
  showsPrec d (C_Char x1) = showString (show (C# x1))
  showsPrec d (CurryChar x1) = case ((\x _ _ -> x) $## x1) (error "Show C_Char: nesting depth used") emptyCs of
    Choice_BinInt _ _ _ _ -> showString "chr " . shows x1
    Choices_BinInt _ _ _  -> showString "chr " . shows x1
    Fail_BinInt _ _       -> shows x1
    Guard_BinInt _ _ _    -> shows x1
    gnfBinInt             -> shows (C# (curryChar2primChar gnfBinInt))

  showList cs | all isPrimChar cs' = showList     (map convert cs')
              | otherwise          = showCharList cs'
   where
    cs' = map gnfCurryChar cs

    gnfCurryChar :: C_Char -> C_Char
    gnfCurryChar (CurryChar x1) = case ((\x _ _ -> x) $## x1) (error "gnfCurryChar: nesting depth used") emptyCs of
      Choice_BinInt _ _ _ _ -> CurryChar x1
      Choices_BinInt _ _ _  -> CurryChar x1
      Fail_BinInt _ _       -> CurryChar x1
      Guard_BinInt _ _ _    -> CurryChar x1
      gnfBinInt             -> C_Char (curryChar2primChar gnfBinInt)
    gnfCurryChar c              = c

    isPrimChar (C_Char _) = True
    isPrimChar _          = False

    convert (C_Char c) = C# c

    showCharList []     = showString "[]"
    showCharList (x:xs) = showChar '[' . shows x . showRest xs
      where
        showRest []     = showChar ']'
        showRest (y:ys) = showChar ',' . shows y . showRest ys

instance Read C_Char where
  readsPrec d s = map readChar (readsPrec d s) where readChar (C# c, s) = (C_Char c, s)

  readList s = map readString (readList s) where readString (cs, s) = (map (\(C# c) -> C_Char c) cs, s)

instance NonDet C_Char where
  choiceCons = Choice_C_Char
  choicesCons = Choices_C_Char
  failCons = Fail_C_Char
  guardCons = Guard_C_Char
  try (Choice_C_Char cd i x y) = tryChoice cd i x y
  try (Choices_C_Char cd i xs) = tryChoices cd i xs
  try (Fail_C_Char cd info) = Fail cd info
  try (Guard_C_Char cd c e) = Guard cd c e
  try x = Val x
  match f _ _ _ _ _ (Choice_C_Char cd i x y) = f cd i x y
  match _ f _ _ _ _ (Choices_C_Char cd i@(NarrowedID _ _) xs) = f cd i xs
  match _ _ f _ _ _ (Choices_C_Char cd i@(FreeID _ _) xs) = f cd i xs
  match _ _ _ _ _ _ (Choices_C_Char cd i _) = error ("Prelude.Char.match: Choices with ChoiceID " ++ (show i))
  match _ _ _ f _ _ (Fail_C_Char cd info) = f cd info
  match _ _ _ _ f _ (Guard_C_Char cd cs e) = f cd cs e
  match _ _ _ _ _ f x = f x

instance Generable C_Char where
  generate s cd = Choices_C_Char cd (freeID [1] s) [CurryChar (generate (leftSupply s) cd)]

instance NormalForm C_Char where
  ($!!) cont x@(C_Char _) cd cs = cont x cd cs
  ($!!) cont (CurryChar x) cd cs = ((cont . CurryChar) $!! x) cd cs
  ($!!) cont (Choice_C_Char d i x y) cd cs = nfChoice cont d i x y cd cs
  ($!!) cont (Choices_C_Char d i xs) cd cs = nfChoices cont d i xs cd cs
  ($!!) cont (Guard_C_Char d c x) cd cs = guardCons d c ((cont $!! x) cd $! (addCs c cs))
  ($!!) _ (Fail_C_Char d info) _ _ = failCons d info
  ($##) cont x@(C_Char _) cd cs = cont x cd cs
  ($##) cont (CurryChar x) cd cs = ((cont . CurryChar) $## x) cd cs
  ($##) cont (Choice_C_Char d i x y) cd cs = gnfChoice cont d i x y cd cs
  ($##) cont (Choices_C_Char d i xs) cd cs = gnfChoices cont d i xs cd cs
  ($##) cont (Guard_C_Char d c x) cd cs = guardCons d c ((cont $## x) cd $! (addCs c cs))
  ($##) _ (Fail_C_Char d info) _ _ = failCons d info
  searchNF search cont c@(C_Char _) = cont c
  searchNF search cont (CurryChar x) = search (cont . CurryChar) x
  searchNF _ _ x = error ("Prelude.Char.searchNF: no constructor: " ++ (show x))

instance Unifiable C_Char where
  (=.=) (C_Char       x1) (C_Char      x2) cd _ | isTrue# (x1 `eqChar#` x2) = C_Success
                                                | otherwise                 = Fail_C_Success cd defFailInfo
  (=.=) (C_Char       x1) (CurryChar x2)   cd cs = (primChar2CurryChar x1 =:= x2) cd cs
  (=.=) (CurryChar  x1) (C_Char      x2)   cd cs = (x1 =:= primChar2CurryChar x2) cd cs
  (=.=) (CurryChar x1)    (CurryChar   x2) cd cs = (x1 =:= x2) cd cs
  (=.=) _                 _                cd _  = Fail_C_Success cd  defFailInfo
  (=.<=) (C_Char       x1) (C_Char      x2) cd _ | isTrue# (x1 `eqChar#` x2) = C_Success
                                                 | otherwise                 = Fail_C_Success cd defFailInfo
  (=.<=) (C_Char       x1) (CurryChar x2)   cd cs = (primChar2CurryChar x1 =:<= x2) cd cs
  (=.<=) (CurryChar  x1) (C_Char      x2)   cd cs = (x1 =:<= primChar2CurryChar x2) cd cs
  (=.<=) (CurryChar x1)    (CurryChar   x2) cd cs = (x1 =:<= x2) cd cs
  (=.<=) _                 _                cd _  = Fail_C_Success cd defFailInfo
  bind cd i (C_Char    x) = (i :=: ChooseN 0 1) : bind cd (leftID i) (primChar2CurryChar x)
  bind cd i (CurryChar x) = (i :=: ChooseN 0 1) : bind cd (leftID i) x
  bind cd i (Choice_C_Char d j l r) = [(ConstraintChoice d j (bind cd i l) (bind cd i r))]
  bind cd i (Choices_C_Char d j@(FreeID _ _) xs) = bindOrNarrow cd i d j xs
  bind cd i (Choices_C_Char d j@(NarrowedID _ _) xs) = [(ConstraintChoices d j (map (bind cd i) xs))]
  bind _  _ c@(Choices_C_Char _ i _) = error ("Prelude.Char.bind: Choices with ChoiceID: " ++ (show c))
  bind _  _ (Fail_C_Char _ info) = [Unsolvable info]
  bind cd  i (Guard_C_Char _ cs e) = getConstrList cs ++ (bind cd i e)
  lazyBind cd i (C_Char    x) = [i :=: ChooseN 0 1, leftID i :=: LazyBind (lazyBind cd (leftID i) (primChar2CurryChar x))]
  lazyBind cd i (CurryChar x) = [i :=: ChooseN 0 1, leftID i :=: LazyBind (lazyBind cd (leftID i) x)]
  lazyBind cd i (Choice_C_Char d j l r) = [(ConstraintChoice d j (lazyBind cd i l) (lazyBind cd i r))]
  lazyBind cd i (Choices_C_Char d j@(FreeID _ _) xs) = lazyBindOrNarrow cd i d j xs
  lazyBind cd i (Choices_C_Char d j@(NarrowedID _ _) xs) = [(ConstraintChoices d j (map (lazyBind cd i) xs))]
  lazyBind _  _ c@(Choices_C_Char _ i _) = error ("Prelude.Char.lazyBind: Choices with ChoiceID: " ++ (show c))
  lazyBind _  _ (Fail_C_Char _ info) = [Unsolvable info]
  lazyBind cd i (Guard_C_Char _ cs e) = getConstrList cs ++ [(i :=: (LazyBind (lazyBind cd i e)))]

instance Curry C_Char where
  (=?=) (Choice_C_Char d i x y) z cd cs = narrow d i ((x =?= z) cd cs) ((y =?= z) cd cs)
  (=?=) (Choices_C_Char d i xs) y cd cs = narrows cs d i (\x -> (x =?= y) cd cs) xs
  (=?=) (Guard_C_Char d c x) y cd cs = guardCons d c ((x =?= y) cd $! (addCs c cs))
  (=?=) (Fail_C_Char d info) _ _ _ = failCons d info
  (=?=) z (Choice_C_Char d i x y) cd cs = narrow d i ((z =?= x) cd cs) ((z =?= y) cd cs)
  (=?=) y (Choices_C_Char d i xs) cd cs = narrows cs d i (\x -> (y =?= x) cd cs) xs
  (=?=) y (Guard_C_Char d c x) cd cs = guardCons d c ((y =?= x) cd $! (addCs c cs))
  (=?=) _ (Fail_C_Char d info) _ _ = failCons d info
  (=?=) (C_Char x1) (C_Char y1) _ _ = toCurry (isTrue# (x1 `eqChar#` y1))
  (=?=) (C_Char      x1) (CurryChar y1) cd cs = ((primChar2CurryChar x1) =?= y1) cd cs
  (=?=) (CurryChar x1) (C_Char      y1) cd cs = (x1 =?= (primChar2CurryChar y1)) cd cs
  (=?=) (CurryChar x1) (CurryChar y1) cd cs = (x1 =?= y1) cd cs
  (<?=) (Choice_C_Char d i x y) z cd cs = narrow d i ((x <?= z) cd cs) ((y <?= z) cd cs)
  (<?=) (Choices_C_Char d i xs) y cd cs = narrows cs d i (\x -> (x <?= y) cd cs) xs
  (<?=) (Guard_C_Char d c x) y cd cs = guardCons d c ((x <?= y) cd $! (addCs c cs))
  (<?=) (Fail_C_Char d info) _ _ _ = failCons d info
  (<?=) z (Choice_C_Char d i x y) cd cs = narrow d i ((z <?= x) cd cs) ((z <?= y) cd cs)
  (<?=) y (Choices_C_Char d i xs) cd cs = narrows cs d i (\x -> (y <?= x) cd cs) xs
  (<?=) y (Guard_C_Char d c x) cd cs = guardCons d c ((y <?= x) cd $! (addCs c cs))
  (<?=) _ (Fail_C_Char d info) _ _ = failCons d info
  (<?=) (C_Char x1) (C_Char y1) _ _ = toCurry (isTrue# (x1 `leChar#` y1))
  (<?=) (C_Char      x1) (CurryChar y1) cd cs = ((primChar2CurryChar x1) `d_C_lteqInteger` y1) cd cs
  (<?=) (CurryChar x1) (C_Char      y1) cd cs = (x1 `d_C_lteqInteger` (primChar2CurryChar y1)) cd cs
  (<?=) (CurryChar x1) (CurryChar y1) cd cs = (x1 `d_C_lteqInteger` y1) cd cs

primChar2CurryChar :: Char# -> BinInt
primChar2CurryChar c = primint2curryint (ord# c)

curryChar2primChar :: BinInt -> Char#
curryChar2primChar c = chr# (curryint2primint c)

-- ---------------------------------------------------------------------------
-- Conversion from and to primitive Haskell types
-- ---------------------------------------------------------------------------

instance ConvertCurryHaskell C_Int Int where
  toCurry (I# i) = C_Int i

  fromCurry (C_Int i)      = I# i
  fromCurry (C_CurryInt i) = I# (curryint2primint i)
  fromCurry _              = error "KiCS2 error: Int data with no ground term"

instance ConvertCurryHaskell C_Int Integer where
  toCurry i = int2C_Int (fromInteger i)
   where
    int2C_Int (I# c) = C_Int c

  fromCurry (C_Int      i) = toInteger (I# i)
  fromCurry (C_CurryInt i) = toInteger (I# (curryint2primint i))
  fromCurry _              = error "KiCS2 error: Int data with no ground term"

instance ConvertCurryHaskell C_Float Float where
  toCurry (F# f) = C_Float f

  fromCurry (C_Float f) = F# f
  fromCurry _           = error "KiCS2 error: Float data with no ground term"

instance ConvertCurryHaskell C_Char Char where
  toCurry (C# c) = C_Char c

  fromCurry (C_Char    c) = C# c
  fromCurry (CurryChar c) = C# (curryChar2primChar c)
  fromCurry _             = error "KiCS2 error: Char data with no ground term"

instance (ConvertCurryHaskell ct ht) =>
         ConvertCurryHaskell (OP_List ct) [ht] where
  toCurry []     = OP_List
  toCurry (c:cs) = OP_Cons (toCurry c) (toCurry cs)

  fromCurry OP_List        = []
  fromCurry (OP_Cons c cs) = fromCurry c : fromCurry cs
  fromCurry _              = error "KiCS2 error: List data with no ground term"

instance ConvertCurryHaskell C_Bool Bool where
  toCurry True  = C_True
  toCurry False = C_False

  fromCurry C_True  = True
  fromCurry C_False = False
  fromCurry _       = error "KiCS2 error: Float data with no ground term"

instance ConvertCurryHaskell OP_Unit () where
  toCurry ()  = OP_Unit

  fromCurry OP_Unit = ()
  fromCurry _       = error "KiCS2 error: Unit data with no ground term"

instance (ConvertCurryHaskell ct1 ht1, ConvertCurryHaskell ct2 ht2) =>
         ConvertCurryHaskell (OP_Tuple2 ct1 ct2) (ht1,ht2) where
  toCurry (x1,x2)  = OP_Tuple2 (toCurry x1) (toCurry x2)

  fromCurry (OP_Tuple2 x1 x2) = (fromCurry x1, fromCurry x2)
  fromCurry _       = error "KiCS2 error: Pair data with no ground term"

instance (ConvertCurryHaskell ct1 ht1, ConvertCurryHaskell ct2 ht2,
          ConvertCurryHaskell ct3 ht3) =>
         ConvertCurryHaskell (OP_Tuple3 ct1 ct2 ct3) (ht1,ht2,ht3) where
  toCurry (x1,x2,x3)  = OP_Tuple3 (toCurry x1) (toCurry x2) (toCurry x3)

  fromCurry (OP_Tuple3 x1 x2 x3) = (fromCurry x1, fromCurry x2, fromCurry x3)
  fromCurry _       = error "KiCS2 error: Tuple3 data with no ground term occurred"

instance ConvertCurryHaskell ct ht =>
         ConvertCurryHaskell (C_Maybe ct) (Maybe ht) where
  toCurry Nothing  = C_Nothing
  toCurry (Just x) = C_Just (toCurry x)

  fromCurry C_Nothing  = Nothing
  fromCurry (C_Just x) = Just (fromCurry x)
  fromCurry _          = error "KiCS2 error: Maybe data with no ground term occurred"

toCurryString :: String -> OP_List C_Char
toCurryString = toCurry

-- -----------------------------------------------------------------------------
-- Auxiliary operations for showing lists
-- -----------------------------------------------------------------------------

showsPrec4CurryList :: Show a => Int -> OP_List a -> ShowS
showsPrec4CurryList d cl =
  if isStandardCurryList cl
  then showsPrec d (clist2hlist cl)
  else showChar '(' . showsPrecRaw d cl . showChar ')'
 where
  isStandardCurryList OP_List = True
  isStandardCurryList (OP_Cons _ xs) = isStandardCurryList xs
  isStandardCurryList _ = False

  clist2hlist OP_List        = []
  clist2hlist (OP_Cons x xs) = x : clist2hlist xs

  showsPrecRaw d (Choice_OP_List cd i x y) = showsChoice d cd i x y
  showsPrecRaw d (Choices_OP_List cd i xs) = showsChoices d cd i xs
  showsPrecRaw d (Guard_OP_List    cd c e) = showsGuard d cd c e
  showsPrecRaw d (Fail_OP_List        _ _) = showChar '!'
  showsPrecRaw d OP_List                   = showString "[]"
  showsPrecRaw d (OP_Cons            x xs) = showParen (d > 5)
    (showsPrec 6 x . showChar ':' . showsPrecRaw 5 xs)

-- -----------------------------------------------------------------------------
-- Primitive operations: General
-- -----------------------------------------------------------------------------

external_d_C_prim_show :: Show a => a -> Cover -> ConstStore -> C_String
external_d_C_prim_show a _ _ = toCurry (show a)

external_d_OP_eq_eq :: Curry a => a -> a -> Cover -> ConstStore -> C_Bool
external_d_OP_eq_eq  = (=?=)

external_d_OP_lt_eq :: Curry a => a -> a -> Cover -> ConstStore -> C_Bool
external_d_OP_lt_eq = (<?=)

external_d_OP_eq_colon_eq :: Unifiable a => a -> a -> Cover -> ConstStore -> C_Success
external_d_OP_eq_colon_eq = (=:=)

external_d_OP_eq_colon_lt_eq :: Curry a => a -> a -> Cover -> ConstStore -> C_Success
external_d_OP_eq_colon_lt_eq = (=:<=)

external_d_C_failed :: NonDet a => Cover -> ConstStore -> a
external_d_C_failed cd _ = failCons cd (customFail "Call to function `failed'")

external_d_C_success :: Cover -> ConstStore -> C_Success
external_d_C_success _ _ = C_Success

external_d_C_cond :: Curry a => C_Success -> a -> Cover -> ConstStore -> a
external_d_C_cond succ a cd cs = ((\_ _ _ -> a) `d_OP_dollar_hash` succ) cd cs

external_d_OP_amp :: C_Success -> C_Success -> Cover -> ConstStore -> C_Success
external_d_OP_amp = (&)

external_d_C_ensureNotFree :: Curry a => a -> Cover -> ConstStore -> a
external_d_C_ensureNotFree x cd cs = case try x of
  Choice   d i a b  -> choiceCons d i (external_d_C_ensureNotFree a cd cs)
                                      (external_d_C_ensureNotFree b cd cs)
  Narrowed d  i xs -> choicesCons d i
                      (map (\x -> external_d_C_ensureNotFree x cd cs) xs)
  Free     d  i xs -> narrows cs d i
                      (\x -> external_d_C_ensureNotFree x cd cs) xs
  Guard    d   c e -> guardCons d c
                      (external_d_C_ensureNotFree e cd $! (addCs c cs))
  _                -> x

external_d_OP_dollar_bang :: (NonDet a, NonDet b)
  => (a -> Cover -> ConstStore -> b) -> a -> Cover -> ConstStore -> b
external_d_OP_dollar_bang = d_dollar_bang

external_nd_OP_dollar_bang :: (NonDet a, NonDet b)
  => (Func a b) -> a -> IDSupply -> Cover -> ConstStore -> b
external_nd_OP_dollar_bang = nd_dollar_bang

external_d_OP_dollar_bang_bang :: (NormalForm a, NonDet b)
  => (a -> Cover -> ConstStore -> b) -> a -> Cover -> ConstStore -> b
external_d_OP_dollar_bang_bang = ($!!)

external_nd_OP_dollar_bang_bang :: (NormalForm a, NonDet b)
  => Func a b -> a -> IDSupply -> Cover -> ConstStore -> b
external_nd_OP_dollar_bang_bang f x s cd cs
  = ((\y cd1 cs1-> nd_apply f y s cd1 cs1) $!! x) cd cs

external_d_OP_dollar_hash_hash :: (NormalForm a, NonDet b)
  => (a -> Cover -> ConstStore -> b) -> a -> Cover -> ConstStore -> b
external_d_OP_dollar_hash_hash = ($##)

external_nd_OP_dollar_hash_hash :: (NormalForm a, NonDet b)
  => Func a b -> a -> IDSupply -> Cover -> ConstStore -> b
external_nd_OP_dollar_hash_hash f x s cd cs
  = ((\y cd1 cs1 -> nd_apply f y s cd1 cs1) $## x) cd cs

external_d_C_apply :: (a -> Cover -> ConstStore -> b) -> a
  -> Cover -> ConstStore -> b
external_d_C_apply = d_apply

external_nd_C_apply :: NonDet b => Func a b -> a
  -> IDSupply -> Cover -> ConstStore -> b
external_nd_C_apply = nd_apply

-- -----------------------------------------------------------------------------
-- Primitive operations: Characters
-- -----------------------------------------------------------------------------

external_d_C_prim_ord :: C_Char -> Cover -> ConstStore -> C_Int
external_d_C_prim_ord (C_Char c)    _ _ = C_Int (ord# c)
external_d_C_prim_ord (CurryChar c) _ _ = C_CurryInt c

external_d_C_prim_chr :: C_Int -> Cover -> ConstStore -> C_Char
external_d_C_prim_chr (C_Int i)      _ _ = C_Char (chr# i)
external_d_C_prim_chr (C_CurryInt i) _ _ = CurryChar i

-- -----------------------------------------------------------------------------
-- Primitive operations: Arithmetics
-- -----------------------------------------------------------------------------

external_d_OP_plus :: C_Int -> C_Int -> Cover -> ConstStore -> C_Int
external_d_OP_plus (C_Int      x) (C_Int      y) _  _  = C_Int (x +# y)
external_d_OP_plus (C_Int      x) (C_CurryInt y) cd cs
  = C_CurryInt (((primint2curryint x) `d_OP_plus_hash` y) cd cs)
external_d_OP_plus (C_CurryInt x) (C_Int      y) cd cs
  = C_CurryInt ((x `d_OP_plus_hash` (primint2curryint y)) cd cs)
external_d_OP_plus (C_CurryInt x) (C_CurryInt y) cd cs
  = C_CurryInt ((x `d_OP_plus_hash` y) cd cs)
external_d_OP_plus x              y              cd cs
  = ((\a cd1 cs1 -> ((\b cd2 cs2 -> ((a `external_d_OP_plus` b) cd2 cs2))
    `d_OP_dollar_hash` y) cd1 cs1) `d_OP_dollar_hash` x) cd cs

external_d_OP_minus :: C_Int -> C_Int -> Cover -> ConstStore -> C_Int
external_d_OP_minus (C_Int      x) (C_Int      y) _  _  = C_Int (x -# y)
external_d_OP_minus (C_Int      x) (C_CurryInt y) cd cs
  = C_CurryInt (((primint2curryint x) `d_OP_minus_hash` y) cd cs)
external_d_OP_minus (C_CurryInt x) (C_Int y)      cd cs
  = C_CurryInt ((x `d_OP_minus_hash` (primint2curryint y)) cd cs)
external_d_OP_minus (C_CurryInt x) (C_CurryInt y) cd cs
  = C_CurryInt ((x `d_OP_minus_hash` y) cd cs)
external_d_OP_minus x              y              cd cs
  = ((\a cd1 cs1 -> ((\b cd2 cs2 -> ((a `external_d_OP_minus` b) cd2 cs2 ))
    `d_OP_dollar_hash` y) cd1 cs1) `d_OP_dollar_hash` x) cd cs

external_d_OP_star :: C_Int -> C_Int -> Cover -> ConstStore -> C_Int
external_d_OP_star (C_Int      x) (C_Int      y) _  _  = C_Int (x *# y)
external_d_OP_star (C_Int      x) (C_CurryInt y) cd cs
  = C_CurryInt (((primint2curryint x) `d_OP_star_hash` y) cd cs)
external_d_OP_star (C_CurryInt x) (C_Int      y) cd cs
  = C_CurryInt ((x `d_OP_star_hash` (primint2curryint y)) cd cs)
external_d_OP_star (C_CurryInt x) (C_CurryInt y) cd cs
  = C_CurryInt ((x `d_OP_star_hash` y) cd cs)
external_d_OP_star x              y              cd cs
  = ((\a cd1 cs1 -> ((\b cd2 cs2 -> ((a `external_d_OP_star` b) cd2 cs2))
    `d_OP_dollar_hash` y) cd1 cs1) `d_OP_dollar_hash` x) cd cs

external_d_C_quot :: C_Int -> C_Int -> Cover -> ConstStore -> C_Int
external_d_C_quot (C_Int      x) (C_Int      y) cd _
  | isTrue# (y ==# 0#) = Fail_C_Int cd (customFail "Division by Zero")
  | otherwise          = C_Int (x `quotInt#` y)
external_d_C_quot (C_Int      x) (C_CurryInt y) cd cs
  = C_CurryInt (((primint2curryint x) `d_C_quotInteger` y) cd cs)
external_d_C_quot (C_CurryInt x) (C_Int      y) cd cs
  = C_CurryInt ((x `d_C_quotInteger` (primint2curryint y)) cd cs)
external_d_C_quot (C_CurryInt x) (C_CurryInt y) cd cs
  = C_CurryInt ((x `d_C_quotInteger` y) cd cs)
external_d_C_quot x              y              cd cs
  = ((\a cd1 cs1 -> ((\b cd2 cs2 -> ((a `external_d_C_quot` b) cd2 cs2 ))
    `d_OP_dollar_hash` y) cd1 cs1) `d_OP_dollar_hash` x) cd cs

external_d_C_rem :: C_Int -> C_Int -> Cover -> ConstStore -> C_Int
external_d_C_rem (C_Int      x) (C_Int      y) cd _
  | isTrue# (y ==# 0#) = Fail_C_Int cd (customFail "Division by Zero")
  | otherwise          = C_Int (x `remInt#` y)
external_d_C_rem (C_Int      x) (C_CurryInt y) cd cs
  = C_CurryInt (((primint2curryint x) `d_C_remInteger` y) cd cs)
external_d_C_rem (C_CurryInt x) (C_Int      y) cd cs
  = C_CurryInt ((x `d_C_remInteger` (primint2curryint y)) cd cs)
external_d_C_rem (C_CurryInt x) (C_CurryInt y) cd cs
  = C_CurryInt ((x `d_C_remInteger` y) cd cs)
external_d_C_rem x              y              cd cs
  = ((\a cd1 cs1 -> ((\b cd2 cs2 -> ((a `external_d_C_rem` b) cd2 cs2))
    `d_OP_dollar_hash` y) cd1 cs1) `d_OP_dollar_hash` x) cd cs

external_d_C_quotRem :: C_Int -> C_Int
  -> Cover -> ConstStore -> OP_Tuple2 C_Int C_Int
external_d_C_quotRem (C_Int      x) (C_Int      y) cd _
  | isTrue# (y ==# 0#)  = Fail_OP_Tuple2 cd (customFail "Division by Zero")
  | otherwise           = OP_Tuple2 (C_Int (x `quotInt#` y)) (C_Int (x `remInt#` y))
external_d_C_quotRem (C_Int      x) (C_CurryInt y) cd cs
  = (mkIntTuple `d_dollar_bang` (((primint2curryint x) `d_C_quotRemInteger` y) cd cs)) cd cs
external_d_C_quotRem (C_CurryInt x) (C_Int      y) cd cs
  = (mkIntTuple `d_dollar_bang` ((x `d_C_quotRemInteger` (primint2curryint y)) cd cs)) cd cs
external_d_C_quotRem (C_CurryInt x) (C_CurryInt y) cd cs
  = (mkIntTuple `d_dollar_bang` ((x `d_C_quotRemInteger` y) cd cs)) cd cs
external_d_C_quotRem x              y              cd cs
  = ((\a cd1 cs1 -> ((\b cd2 cs2 -> ((a `external_d_C_quotRem` b) cd2 cs2))
    `d_OP_dollar_hash` y) cd1 cs1) `d_OP_dollar_hash` x) cd cs

external_d_C_div :: C_Int -> C_Int -> Cover -> ConstStore -> C_Int
external_d_C_div (C_Int      x) (C_Int      y) cd _
  | isTrue# (y ==# 0#) = Fail_C_Int cd (customFail "Division by Zero")
  | otherwise          = C_Int (x `divInt#` y)
external_d_C_div (C_Int      x) (C_CurryInt y) cd cs
  = C_CurryInt (((primint2curryint x) `d_C_divInteger` y) cd cs)
external_d_C_div (C_CurryInt x) (C_Int      y) cd cs
  = C_CurryInt ((x `d_C_divInteger` (primint2curryint y)) cd cs)
external_d_C_div (C_CurryInt x) (C_CurryInt y) cd cs
  = C_CurryInt ((x `d_C_divInteger` y) cd cs)
external_d_C_div x              y              cd cs
  = ((\a cd1 cs1-> ((\b cd2 cs2-> ((a `external_d_C_div` b) cd2 cs2))
    `d_OP_dollar_hash` y) cd1 cs1) `d_OP_dollar_hash` x) cd cs

-- PrimOp taken from GHC.Base
divInt# :: Int# -> Int# -> Int#
x# `divInt#` y#
        -- Be careful NOT to overflow if we do any additional arithmetic
        -- on the arguments...  the following  previous version of this
        -- code has problems with overflow:
--    | (x# ># 0#) && (y# <# 0#) = ((x# -# y#) -# 1#) `quotInt#` y#
--    | (x# <# 0#) && (y# ># 0#) = ((x# -# y#) +# 1#) `quotInt#` y#
    | isTrue# ((x# ># 0#)) && isTrue# ((y# <# 0#)) = ((x# -# 1#) `quotInt#` y#) -# 1#
    | isTrue# ((x# <# 0#)) && isTrue# ((y# ># 0#)) = ((x# +# 1#) `quotInt#` y#) -# 1#
    | otherwise                                    = x# `quotInt#` y#

external_d_C_mod :: C_Int -> C_Int -> Cover -> ConstStore -> C_Int
external_d_C_mod (C_Int      x) (C_Int      y) cd _
  | isTrue# (y ==# 0#) = Fail_C_Int cd (customFail "Division by Zero")
  | otherwise          = C_Int (x `modInt#` y)
external_d_C_mod (C_Int      x) (C_CurryInt y) cd cs
  = C_CurryInt (((primint2curryint x) `d_C_modInteger` y) cd cs)
external_d_C_mod (C_CurryInt x) (C_Int      y) cd cs
  = C_CurryInt ((x `d_C_modInteger` (primint2curryint y)) cd cs)
external_d_C_mod (C_CurryInt x) (C_CurryInt y) cd cs
  = C_CurryInt ((x `d_C_modInteger` y) cd cs)
external_d_C_mod x              y              cd cs
  = ((\a cd1 cs1 -> ((\b cd2 cs2 -> ((a `external_d_C_mod` b)) cd2 cs2)
    `d_OP_dollar_hash` y) cd1 cs1) `d_OP_dollar_hash` x) cd cs

-- PrimOp taken from GHC.Base
modInt# :: Int# -> Int# -> Int#
x# `modInt#` y#
    | isTrue# ((x# ># 0#)) && isTrue# ((y# <# 0#)) ||
      isTrue# ((x# <# 0#)) && isTrue# ((y# ># 0#))
    = if isTrue# (r# /=# 0#) then r# +# y# else 0#
    | otherwise
    = r#
  where !r# = x# `remInt#` y#

external_d_C_divMod :: C_Int -> C_Int ->  Cover -> ConstStore -> OP_Tuple2 C_Int C_Int
external_d_C_divMod (C_Int      x) (C_Int      y) cd _
  | isTrue# (y ==# 0#) = Fail_OP_Tuple2 cd (customFail "Division by Zero")
  | otherwise          = OP_Tuple2 (C_Int (x `divInt#` y)) (C_Int (x `modInt#` y))
external_d_C_divMod (C_Int      x) (C_CurryInt y) cd cs
  = (mkIntTuple `d_OP_dollar_hash` (((primint2curryint x) `d_C_divModInteger` y) cd cs)) cd cs
external_d_C_divMod (C_CurryInt x) (C_Int      y) cd cs
  = (mkIntTuple `d_OP_dollar_hash` ((x `d_C_divModInteger` (primint2curryint y)) cd cs)) cd cs
external_d_C_divMod (C_CurryInt x) (C_CurryInt y) cd cs
  = (mkIntTuple `d_OP_dollar_hash` ((x `d_C_divModInteger` y) cd cs)) cd cs
external_d_C_divMod x              y              cd cs
  = ((\a cd1 cs1 -> ((\b cd2 cs2 -> ((a `external_d_C_divMod` b) cd2 cs2 ))
    `d_OP_dollar_hash` y) cd1 cs1) `d_OP_dollar_hash` x) cd cs

mkIntTuple :: OP_Tuple2 BinInt BinInt -> Cover -> ConstStore -> OP_Tuple2 C_Int C_Int
mkIntTuple (OP_Tuple2 d m) _ _ = OP_Tuple2 (C_CurryInt d) (C_CurryInt m)

external_d_C_negateFloat :: C_Float -> Cover -> ConstStore -> C_Float
external_d_C_negateFloat (C_Float x) _  _  = C_Float (negateFloat# x)
external_d_C_negateFloat x           cd cs
  = (external_d_C_negateFloat `d_OP_dollar_hash` x) cd cs

-- -----------------------------------------------------------------------------
-- Primitive operations: IO stuff
-- -----------------------------------------------------------------------------

external_d_C_return :: a -> Cover -> ConstStore -> C_IO a
external_d_C_return a _ _ = fromIO (return a)

external_d_C_prim_putChar :: C_Char -> Cover -> ConstStore -> C_IO OP_Unit
external_d_C_prim_putChar c _ _ = toCurry putChar c

external_d_C_getChar :: Cover -> ConstStore -> C_IO C_Char
external_d_C_getChar _ _ = toCurry getChar

external_d_C_prim_readFile :: C_String -> Cover -> ConstStore -> C_IO C_String
external_d_C_prim_readFile s _ _ = toCurry readFile s

-- TODO: Problem: s is not evaluated to enable lazy IO and therefore could
-- be non-deterministic
external_d_C_prim_writeFile :: C_String -> C_String
  -> Cover -> ConstStore -> C_IO OP_Unit
external_d_C_prim_writeFile s1 s2 _ _ = toCurry writeFile s1 s2

-- TODO: Problem: s is not evaluated to enable lazy IO and therefore could
-- be non-deterministic
external_d_C_prim_appendFile :: C_String -> C_String
  -> Cover -> ConstStore -> C_IO OP_Unit
external_d_C_prim_appendFile s1 s2 _ _ = toCurry appendFile s1 s2

external_d_OP_gt_gt_eq :: (Curry t0, Curry t1)
  => C_IO t0 -> (t0 -> Cover -> ConstStore -> C_IO t1)
  -> Cover -> ConstStore -> C_IO t1
external_d_OP_gt_gt_eq m f cd cs = C_IO $ do
  res <- searchIO errSupply cd cs m
  case res of
    Left err -> return (Left (traceFail ("Prelude.>>=") [show m, show f] err))
    Right x  -> do
    cs1 <- lookupGlobalCs
    let cs2 = combineCs cs cs1
    searchIO errSupply cd cs2 (f x cd cs2)
  where errSupply = internalError "Prelude.(>>=): ID supply used"

-- TODO: Investigate if `cs` and `cs'` are in a subset relation
-- in either direction.
external_nd_OP_gt_gt_eq :: (Curry t0, Curry t1)
  => C_IO t0 -> Func t0 (C_IO t1)
  -> IDSupply -> Cover -> ConstStore -> C_IO t1
external_nd_OP_gt_gt_eq m f _ _ cs = HO_C_IO $ \s cd cs' -> do
  let cs1 = combineCs cs' cs
  res <- searchIO (leftSupply s) cd cs1 m
  case res of
    Left err -> return (Left (traceFail ("Prelude.>>=") [show m, show f] err))
    Right x  -> do
    cs2 <- lookupGlobalCs
    let cs3 = combineCs cs1 cs2
        s'  = rightSupply s
    searchIO (leftSupply s') cd cs3 (nd_apply f x (rightSupply s') cd cs3)

-- -----------------------------------------------------------------------------
-- Primitive operations: Exception handling
-- -----------------------------------------------------------------------------

instance ConvertCurryHaskell C_IOError CurryException where
  toCurry (IOException     s) = C_IOError     (toCurry s)
  toCurry (UserException   s) = C_UserError   (toCurry s)
  toCurry (FailException   s) = C_FailError   (toCurry s)
  toCurry (NondetException s) = C_NondetError (toCurry s)

  fromCurry (C_IOError     s) = IOException     $ fromCurry s
  fromCurry (C_UserError   s) = UserException   $ fromCurry s
  fromCurry (C_FailError   s) = FailException   $ fromCurry s
  fromCurry (C_NondetError s) = NondetException $ fromCurry s
  fromCurry _                 = internalError "non-deterministic IOError"

external_d_C_prim_error :: C_String -> Cover -> ConstStore -> a
external_d_C_prim_error s _ _ = C.throw $ UserException (fromCurry s)

external_d_C_prim_ioError :: C_IOError -> Cover -> ConstStore -> C_IO a
external_d_C_prim_ioError e _ _ = C.throw $ (fromCurry e :: CurryException)

external_d_C_catch :: C_IO a -> (C_IOError -> Cover -> ConstStore -> C_IO a)
  -> Cover -> ConstStore -> C_IO a
external_d_C_catch act hndl cd cs =
  fromIO $ C.catches (toIO errSupply1 cd cs act)
                     (exceptionHandlers errSupply2 cd cs (nd hndl))
  where
  errSupply1 = internalError "Prelude.catch: ID supply 1 used"
  errSupply2 = internalError "Prelude.catch: ID supply 2 used"

external_nd_C_catch :: C_IO a -> Func C_IOError (C_IO a)
  -> IDSupply -> Cover -> ConstStore -> C_IO a
external_nd_C_catch act hndl _ _ cs = HO_C_IO $ \s cd cs' -> do
  let cs1 = combineCs cs' cs
  res <- C.catches (toIO (leftSupply s) cd cs1 act)
                   (exceptionHandlers (rightSupply s) cd cs1 (nd_apply hndl))
  return (Right res)

exceptionHandlers :: IDSupply -> Cover -> ConstStore -> (C_IOError -> IDSupply -> Cover -> ConstStore -> C_IO a) -> [C.Handler a]
exceptionHandlers s cd cs hndl =
  [ C.Handler (\ (e :: CurryException) -> toIO (leftSupply s) cd cs (hndl (toCurry         e) (rightSupply s) cd cs))
  , C.Handler (\ (e ::  C.IOException) -> toIO (leftSupply s) cd cs (hndl (fromIOException e) (rightSupply s) cd cs))
  ] where fromIOException = toCurry . IOException . show

-- -----------------------------------------------------------------------------
-- Functions on Integer and Nat added from PrimTypes
-- -----------------------------------------------------------------------------

instance Curry Nat where
  (=?=) (Choice_Nat cd i x y) z d cs = narrow cd i (((x =?= z) d) cs) (((y =?= z) d) cs)
  (=?=) (Choices_Nat  cd i xs) y d cs = narrows cs cd i (\x -> ((x =?= y) d) cs) xs
  (=?=) (Guard_Nat  cd c e) y d cs = guardCons cd c (((e =?= y) d) (addCs c cs))
  (=?=) (Fail_Nat  cd info) _ _ _ = failCons cd info
  (=?=) z (Choice_Nat  cd i x y) d cs = narrow cd i (((z =?= x) d) cs) (((z =?= y) d) cs)
  (=?=) y (Choices_Nat  cd i xs) d cs = narrows cs cd i (\x -> ((y =?= x) d) cs) xs
  (=?=) y (Guard_Nat  cd c e) d cs = guardCons cd c (((y =?= e) d) (addCs c cs))
  (=?=) _ (Fail_Nat  cd info) _ _ = failCons cd info
  (=?=) IHi IHi d cs = C_True
  (=?=) (O x1) (O y1) d cs = ((x1 =?= y1) d) cs
  (=?=) (I x1) (I y1) d cs = ((x1 =?= y1) d) cs
  (=?=) _ _ d _ = C_False
  (<?=) (Choice_Nat  cd i x y) z d cs = narrow cd i (((x <?= z) d) cs) (((y <?= z) d) cs)
  (<?=) (Choices_Nat  cd i xs) y d cs = narrows cs cd i (\x -> ((x <?= y) d) cs) xs
  (<?=) (Guard_Nat  cd c e) y d cs = guardCons cd c (((e <?= y) d) (addCs c cs))
  (<?=) (Fail_Nat  cd info) _ _ _ = failCons cd info
  (<?=) z (Choice_Nat  cd i x y) d cs = narrow cd i (((z <?= x) d) cs) (((z <?= y) d) cs)
  (<?=) y (Choices_Nat  cd i xs) d cs = narrows cs cd i (\x -> ((y <?= x) d) cs) xs
  (<?=) y (Guard_Nat  cd c e) d cs = guardCons cd c (((y <?= e) d) (addCs c cs))
  (<?=) _ (Fail_Nat  cd info) _ _ = failCons cd info
  (<?=) IHi IHi d cs = C_True
  (<?=) IHi (O _) _ _ = C_True
  (<?=) IHi (I _) _ _ = C_True
  (<?=) (O x1) (O y1) d cs = ((x1 <?= y1) d) cs
  (<?=) (O _) (I _) _ _ = C_True
  (<?=) (I x1) (I y1) d cs = ((x1 <?= y1) d) cs
  (<?=) _ _ d _ = C_False


instance Curry BinInt where
  (=?=) (Choice_BinInt cd i x y) z d cs = narrow cd i (((x =?= z) d) cs) (((y =?= z) d) cs)
  (=?=) (Choices_BinInt cd i xs) y d cs = narrows cs cd i (\x -> ((x =?= y) d) cs) xs
  (=?=) (Guard_BinInt cd c e) y d cs = guardCons cd c (((e =?= y) d) (addCs c cs))
  (=?=) (Fail_BinInt cd info) _ _ _ = failCons cd info
  (=?=) z (Choice_BinInt cd i x y) d cs = narrow cd i (((z =?= x) d) cs) (((z =?= y) d) cs)
  (=?=) y (Choices_BinInt cd i xs) d cs = narrows cs cd i (\x -> ((y =?= x) d) cs) xs
  (=?=) y (Guard_BinInt cd c e) d cs = guardCons cd c (((y =?= e) d) (addCs c cs))
  (=?=) _ (Fail_BinInt cd info) _ _ = failCons cd info
  (=?=) (Neg x1) (Neg y1) d cs = ((x1 =?= y1) d) cs
  (=?=) Zero Zero d cs = C_True
  (=?=) (Pos x1) (Pos y1) d cs = ((x1 =?= y1) d) cs
  (=?=) _ _ d _ = C_False
  (<?=) (Choice_BinInt cd i x y) z d cs = narrow cd i (((x <?= z) d) cs) (((y <?= z) d) cs)
  (<?=) (Choices_BinInt cd i xs) y d cs = narrows cs cd i (\x -> ((x <?= y) d) cs) xs
  (<?=) (Guard_BinInt cd c e) y d cs = guardCons cd c (((e <?= y) d) (addCs c cs))
  (<?=) (Fail_BinInt cd info) _ _ _ = failCons cd info
  (<?=) z (Choice_BinInt cd i x y) d cs = narrow cd i (((z <?= x) d) cs) (((z <?= y) d) cs)
  (<?=) y (Choices_BinInt cd i xs) d cs = narrows cs cd i (\x -> ((y <?= x) d) cs) xs
  (<?=) y (Guard_BinInt cd c e) d cs = guardCons cd c (((y <?= e) d) (addCs c cs))
  (<?=) _ (Fail_BinInt cd info) _ _ = failCons cd info
  (<?=) (Neg x1) (Neg y1) d cs = ((x1 <?= y1) d) cs
  (<?=) (Neg _) Zero _ _ = C_True
  (<?=) (Neg _) (Pos _) _ _ = C_True
  (<?=) Zero Zero d cs = C_True
  (<?=) Zero (Pos _) _ _ = C_True
  (<?=) (Pos x1) (Pos y1) d cs = ((x1 <?= y1) d) cs
  (<?=) _ _ d _ = C_False

d_C_cmpNat :: Nat  -> Nat  -> Cover -> ConstStore -> C_Ordering
d_C_cmpNat x1 x2 x3250 x3500 = case x1 of
     IHi -> d_OP__casePT_33 x2 x3250 x3500
     (O x5) -> d_OP__casePT_32 x5 x2 x3250 x3500
     (I x8) -> d_OP__casePT_30 x8 x2 x3250 x3500
     (Choice_Nat  x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_cmpNat x1002 x2 x3250 x3500) (d_C_cmpNat x1003 x2 x3250 x3500)
     (Choices_Nat  x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_cmpNat z x2 x3250 x3500) x1002
     (Guard_Nat  x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_cmpNat x1002 x2 x3250) $! (addCs x1001 x3500))
     (Fail_Nat  x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_succ :: Nat  -> Cover -> ConstStore -> Nat
d_C_succ x1 x3250 x3500 = case x1 of
     IHi -> O IHi
     (O x2) -> I x2
     (I x3) -> O (d_C_succ x3 x3250 x3500)
     (Choice_Nat  x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_succ x1002 x3250 x3500) (d_C_succ x1003 x3250 x3500)
     (Choices_Nat  x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_succ z x3250 x3500) x1002
     (Guard_Nat  x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_succ x1002 x3250) $! (addCs x1001 x3500))
     (Fail_Nat  x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_pred :: Nat  -> Cover -> ConstStore -> Nat
d_C_pred x1 x3250 x3500 = case x1 of
     IHi -> d_C_failed x3250 x3500
     (O x2) -> d_OP__casePT_28 x2 x3250 x3500
     (I x5) -> O x5
     (Choice_Nat  x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_pred x1002 x3250 x3500) (d_C_pred x1003 x3250 x3500)
     (Choices_Nat  x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_pred z x3250 x3500) x1002
     (Guard_Nat  x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_pred x1002 x3250) $! (addCs x1001 x3500))
     (Fail_Nat  x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_plus_caret :: Nat  -> Nat  -> Cover -> ConstStore -> Nat
d_OP_plus_caret x1 x2 x3250 x3500 = case x1 of
     IHi -> d_C_succ x2 x3250 x3500
     (O x3) -> d_OP__casePT_27 x3 x2 x3250 x3500
     (I x6) -> d_OP__casePT_26 x6 x2 x3250 x3500
     (Choice_Nat  x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_plus_caret x1002 x2 x3250 x3500) (d_OP_plus_caret x1003 x2 x3250 x3500)
     (Choices_Nat  x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_plus_caret z x2 x3250 x3500) x1002
     (Guard_Nat  x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_plus_caret x1002 x2 x3250) $! (addCs x1001 x3500))
     (Fail_Nat  x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_minus_caret :: Nat  -> Nat  -> Cover -> ConstStore -> BinInt
d_OP_minus_caret x1 x2 x3250 x3500 = case x1 of
     IHi -> d_C_inc (Neg x2) x3250 x3500
     (O x3) -> d_OP__casePT_25 x1 x3 x2 x3250 x3500
     (I x6) -> d_OP__casePT_24 x6 x2 x3250 x3500
     (Choice_Nat  x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_minus_caret x1002 x2 x3250 x3500) (d_OP_minus_caret x1003 x2 x3250 x3500)
     (Choices_Nat  x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_minus_caret z x2 x3250 x3500) x1002
     (Guard_Nat  x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_minus_caret x1002 x2 x3250) $! (addCs x1001 x3500))
     (Fail_Nat  x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_mult2 :: BinInt -> Cover -> ConstStore -> BinInt
d_C_mult2 x1 x3250 x3500 = case x1 of
     (Pos x2) -> Pos (O x2)
     Zero -> Zero
     (Neg x3) -> Neg (O x3)
     (Choice_BinInt x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_mult2 x1002 x3250 x3500) (d_C_mult2 x1003 x3250 x3500)
     (Choices_BinInt x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_mult2 z x3250 x3500) x1002
     (Guard_BinInt x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_mult2 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_BinInt x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_star_caret :: Nat  -> Nat  -> Cover -> ConstStore -> Nat
d_OP_star_caret x1 x2 x3250 x3500 = case x1 of
     IHi -> x2
     (O x3) -> O (d_OP_star_caret x3 x2 x3250 x3500)
     (I x4) -> d_OP_plus_caret x2 (O (d_OP_star_caret x4 x2 x3250 x3500)) x3250 x3500
     (Choice_Nat  x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_star_caret x1002 x2 x3250 x3500) (d_OP_star_caret x1003 x2 x3250 x3500)
     (Choices_Nat  x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_star_caret z x2 x3250 x3500) x1002
     (Guard_Nat  x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_star_caret x1002 x2 x3250) $! (addCs x1001 x3500))
     (Fail_Nat  x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_div2 :: Nat  -> Cover -> ConstStore -> Nat
d_C_div2 x1 x3250 x3500 = case x1 of
     IHi -> d_C_failed x3250 x3500
     (O x2) -> x2
     (I x3) -> x3
     (Choice_Nat  x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_div2 x1002 x3250 x3500) (d_C_div2 x1003 x3250 x3500)
     (Choices_Nat  x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_div2 z x3250 x3500) x1002
     (Guard_Nat  x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_div2 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_Nat  x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_mod2 :: Nat  -> Cover -> ConstStore -> BinInt
d_C_mod2 x1 x3250 x3500 = case x1 of
     IHi -> Pos IHi
     (O x2) -> Zero
     (I x3) -> Pos IHi
     (Choice_Nat  x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_mod2 x1002 x3250 x3500) (d_C_mod2 x1003 x3250 x3500)
     (Choices_Nat  x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_mod2 z x3250 x3500) x1002
     (Guard_Nat  x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_mod2 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_Nat  x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_quotRemNat :: Nat  -> Nat  -> Cover -> ConstStore -> OP_Tuple2 BinInt BinInt
d_C_quotRemNat x1 x2 x3250 x3500 = d_OP__casePT_23 x1 x2 (d_OP_eq_eq x2 IHi x3250 x3500) x3250 x3500

d_OP_quotRemNat_dot_shift_dot_104 :: Nat  -> Nat  -> Cover -> ConstStore -> Nat
d_OP_quotRemNat_dot_shift_dot_104 x1 x2 x3250 x3500 = case x1 of
     (O x3) -> O x2
     (I x4) -> I x2
     (Choice_Nat  x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_quotRemNat_dot_shift_dot_104 x1002 x2 x3250 x3500) (d_OP_quotRemNat_dot_shift_dot_104 x1003 x2 x3250 x3500)
     (Choices_Nat  x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_quotRemNat_dot_shift_dot_104 z x2 x3250 x3500) x1002
     (Guard_Nat  x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_quotRemNat_dot_shift_dot_104 x1002 x2 x3250) $! (addCs x1001 x3500))
     (Fail_Nat  x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_lteqInteger :: BinInt -> BinInt -> Cover -> ConstStore -> C_Bool
d_C_lteqInteger x1 x2 x3250 x3500 = d_OP_slash_eq (d_C_cmpInteger x1 x2 x3250 x3500) C_GT x3250 x3500

d_C_cmpInteger :: BinInt -> BinInt -> Cover -> ConstStore -> C_Ordering
d_C_cmpInteger x1 x2 x3250 x3500 = case x1 of
     Zero -> d_OP__casePT_14 x2 x3250 x3500
     (Pos x5) -> d_OP__casePT_13 x5 x2 x3250 x3500
     (Neg x8) -> d_OP__casePT_12 x8 x2 x3250 x3500
     (Choice_BinInt x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_cmpInteger x1002 x2 x3250 x3500) (d_C_cmpInteger x1003 x2 x3250 x3500)
     (Choices_BinInt x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_cmpInteger z x2 x3250 x3500) x1002
     (Guard_BinInt x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_cmpInteger x1002 x2 x3250) $! (addCs x1001 x3500))
     (Fail_BinInt x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_neg :: BinInt -> Cover -> ConstStore -> BinInt
d_C_neg x1 x3250 x3500 = case x1 of
     Zero -> Zero
     (Pos x2) -> Neg x2
     (Neg x3) -> Pos x3
     (Choice_BinInt x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_neg x1002 x3250 x3500) (d_C_neg x1003 x3250 x3500)
     (Choices_BinInt x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_neg z x3250 x3500) x1002
     (Guard_BinInt x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_neg x1002 x3250) $! (addCs x1001 x3500))
     (Fail_BinInt x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_inc :: BinInt -> Cover -> ConstStore -> BinInt
d_C_inc x1 x3250 x3500 = case x1 of
     Zero -> Pos IHi
     (Pos x2) -> Pos (d_C_succ x2 x3250 x3500)
     (Neg x3) -> d_OP__casePT_11 x3 x3250 x3500
     (Choice_BinInt x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_inc x1002 x3250 x3500) (d_C_inc x1003 x3250 x3500)
     (Choices_BinInt x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_inc z x3250 x3500) x1002
     (Guard_BinInt x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_inc x1002 x3250) $! (addCs x1001 x3500))
     (Fail_BinInt x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_dec :: BinInt -> Cover -> ConstStore -> BinInt
d_C_dec x1 x3250 x3500 = case x1 of
     Zero -> Neg IHi
     (Pos x2) -> d_OP__casePT_10 x2 x3250 x3500
     (Neg x5) -> Neg (d_C_succ x5 x3250 x3500)
     (Choice_BinInt x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_dec x1002 x3250 x3500) (d_C_dec x1003 x3250 x3500)
     (Choices_BinInt x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_dec z x3250 x3500) x1002
     (Guard_BinInt x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_dec x1002 x3250) $! (addCs x1001 x3500))
     (Fail_BinInt x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_plus_hash :: BinInt -> BinInt -> Cover -> ConstStore -> BinInt
d_OP_plus_hash x1 x2 x3250 x3500 = case x1 of
     Zero -> x2
     (Pos x3) -> d_OP__casePT_9 x1 x3 x2 x3250 x3500
     (Neg x6) -> d_OP__casePT_8 x1 x6 x2 x3250 x3500
     (Choice_BinInt x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_plus_hash x1002 x2 x3250 x3500) (d_OP_plus_hash x1003 x2 x3250 x3500)
     (Choices_BinInt x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_plus_hash z x2 x3250 x3500) x1002
     (Guard_BinInt x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_plus_hash x1002 x2 x3250) $! (addCs x1001 x3500))
     (Fail_BinInt x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_minus_hash :: BinInt -> BinInt -> Cover -> ConstStore -> BinInt
d_OP_minus_hash x1 x2 x3250 x3500 = case x2 of
     Zero -> x1
     (Pos x3) -> d_OP_plus_hash x1 (Neg x3) x3250 x3500
     (Neg x4) -> d_OP_plus_hash x1 (Pos x4) x3250 x3500
     (Choice_BinInt x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_minus_hash x1 x1002 x3250 x3500) (d_OP_minus_hash x1 x1003 x3250 x3500)
     (Choices_BinInt x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_minus_hash x1 z x3250 x3500) x1002
     (Guard_BinInt x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_minus_hash x1 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_BinInt x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_star_hash :: BinInt -> BinInt -> Cover -> ConstStore -> BinInt
d_OP_star_hash x1 x2 x3250 x3500 = case x1 of
     Zero -> Zero
     (Pos x3) -> d_OP__casePT_7 x3 x2 x3250 x3500
     (Neg x6) -> d_OP__casePT_6 x6 x2 x3250 x3500
     (Choice_BinInt x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_star_hash x1002 x2 x3250 x3500) (d_OP_star_hash x1003 x2 x3250 x3500)
     (Choices_BinInt x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_star_hash z x2 x3250 x3500) x1002
     (Guard_BinInt x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_star_hash x1002 x2 x3250) $! (addCs x1001 x3500))
     (Fail_BinInt x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_quotRemInteger :: BinInt -> BinInt -> Cover -> ConstStore -> OP_Tuple2 BinInt BinInt
d_C_quotRemInteger x1 x2 x3250 x3500 = case x2 of
     Zero -> d_C_failed x3250 x3500
     (Pos x3) -> d_OP__casePT_5 x3 x1 x3250 x3500
     (Neg x9) -> d_OP__casePT_4 x9 x1 x3250 x3500
     (Choice_BinInt x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_quotRemInteger x1 x1002 x3250 x3500) (d_C_quotRemInteger x1 x1003 x3250 x3500)
     (Choices_BinInt x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_quotRemInteger x1 z x3250 x3500) x1002
     (Guard_BinInt x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_quotRemInteger x1 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_BinInt x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_quotRemInteger_dot___hash_selFP2_hash_d :: OP_Tuple2 BinInt BinInt -> Cover -> ConstStore -> BinInt
d_OP_quotRemInteger_dot___hash_selFP2_hash_d x1 x3250 x3500 = case x1 of
     (OP_Tuple2 x2 x3) -> x2
     (Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_quotRemInteger_dot___hash_selFP2_hash_d x1002 x3250 x3500) (d_OP_quotRemInteger_dot___hash_selFP2_hash_d x1003 x3250 x3500)
     (Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_quotRemInteger_dot___hash_selFP2_hash_d z x3250 x3500) x1002
     (Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_quotRemInteger_dot___hash_selFP2_hash_d x1002 x3250) $! (addCs x1001 x3500))
     (Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_quotRemInteger_dot___hash_selFP3_hash_m :: OP_Tuple2 BinInt BinInt -> Cover -> ConstStore -> BinInt
d_OP_quotRemInteger_dot___hash_selFP3_hash_m x1 x3250 x3500 = case x1 of
     (OP_Tuple2 x2 x3) -> x3
     (Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_quotRemInteger_dot___hash_selFP3_hash_m x1002 x3250 x3500) (d_OP_quotRemInteger_dot___hash_selFP3_hash_m x1003 x3250 x3500)
     (Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_quotRemInteger_dot___hash_selFP3_hash_m z x3250 x3500) x1002
     (Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_quotRemInteger_dot___hash_selFP3_hash_m x1002 x3250) $! (addCs x1001 x3500))
     (Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_quotRemInteger_dot___hash_selFP5_hash_d :: OP_Tuple2 BinInt BinInt -> Cover -> ConstStore -> BinInt
d_OP_quotRemInteger_dot___hash_selFP5_hash_d x1 x3250 x3500 = case x1 of
     (OP_Tuple2 x2 x3) -> x2
     (Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_quotRemInteger_dot___hash_selFP5_hash_d x1002 x3250 x3500) (d_OP_quotRemInteger_dot___hash_selFP5_hash_d x1003 x3250 x3500)
     (Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_quotRemInteger_dot___hash_selFP5_hash_d z x3250 x3500) x1002
     (Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_quotRemInteger_dot___hash_selFP5_hash_d x1002 x3250) $! (addCs x1001 x3500))
     (Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_quotRemInteger_dot___hash_selFP6_hash_m :: OP_Tuple2 BinInt BinInt -> Cover -> ConstStore -> BinInt
d_OP_quotRemInteger_dot___hash_selFP6_hash_m x1 x3250 x3500 = case x1 of
     (OP_Tuple2 x2 x3) -> x3
     (Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_quotRemInteger_dot___hash_selFP6_hash_m x1002 x3250 x3500) (d_OP_quotRemInteger_dot___hash_selFP6_hash_m x1003 x3250 x3500)
     (Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_quotRemInteger_dot___hash_selFP6_hash_m z x3250 x3500) x1002
     (Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_quotRemInteger_dot___hash_selFP6_hash_m x1002 x3250) $! (addCs x1001 x3500))
     (Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_quotRemInteger_dot___hash_selFP8_hash_d :: OP_Tuple2 BinInt BinInt -> Cover -> ConstStore -> BinInt
d_OP_quotRemInteger_dot___hash_selFP8_hash_d x1 x3250 x3500 = case x1 of
     (OP_Tuple2 x2 x3) -> x2
     (Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_quotRemInteger_dot___hash_selFP8_hash_d x1002 x3250 x3500) (d_OP_quotRemInteger_dot___hash_selFP8_hash_d x1003 x3250 x3500)
     (Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_quotRemInteger_dot___hash_selFP8_hash_d z x3250 x3500) x1002
     (Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_quotRemInteger_dot___hash_selFP8_hash_d x1002 x3250) $! (addCs x1001 x3500))
     (Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_quotRemInteger_dot___hash_selFP9_hash_m :: OP_Tuple2 BinInt BinInt -> Cover -> ConstStore -> BinInt
d_OP_quotRemInteger_dot___hash_selFP9_hash_m x1 x3250 x3500 = case x1 of
     (OP_Tuple2 x2 x3) -> x3
     (Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_quotRemInteger_dot___hash_selFP9_hash_m x1002 x3250 x3500) (d_OP_quotRemInteger_dot___hash_selFP9_hash_m x1003 x3250 x3500)
     (Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_quotRemInteger_dot___hash_selFP9_hash_m z x3250 x3500) x1002
     (Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_quotRemInteger_dot___hash_selFP9_hash_m x1002 x3250) $! (addCs x1001 x3500))
     (Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_divModInteger :: BinInt -> BinInt -> Cover -> ConstStore -> OP_Tuple2 BinInt BinInt
d_C_divModInteger x1 x2 x3250 x3500 = case x2 of
     Zero -> d_C_failed x3250 x3500
     (Pos x3) -> d_OP__casePT_3 x3 x1 x3250 x3500
     (Neg x11) -> d_OP__casePT_1 x11 x1 x3250 x3500
     (Choice_BinInt x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_divModInteger x1 x1002 x3250 x3500) (d_C_divModInteger x1 x1003 x3250 x3500)
     (Choices_BinInt x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_divModInteger x1 z x3250 x3500) x1002
     (Guard_BinInt x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_divModInteger x1 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_BinInt x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_divModInteger_dot___hash_selFP11_hash_d :: OP_Tuple2 BinInt BinInt -> Cover -> ConstStore -> BinInt
d_OP_divModInteger_dot___hash_selFP11_hash_d x1 x3250 x3500 = case x1 of
     (OP_Tuple2 x2 x3) -> x2
     (Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_divModInteger_dot___hash_selFP11_hash_d x1002 x3250 x3500) (d_OP_divModInteger_dot___hash_selFP11_hash_d x1003 x3250 x3500)
     (Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_divModInteger_dot___hash_selFP11_hash_d z x3250 x3500) x1002
     (Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_divModInteger_dot___hash_selFP11_hash_d x1002 x3250) $! (addCs x1001 x3500))
     (Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_divModInteger_dot___hash_selFP12_hash_m :: OP_Tuple2 BinInt BinInt -> Cover -> ConstStore -> BinInt
d_OP_divModInteger_dot___hash_selFP12_hash_m x1 x3250 x3500 = case x1 of
     (OP_Tuple2 x2 x3) -> x3
     (Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_divModInteger_dot___hash_selFP12_hash_m x1002 x3250 x3500) (d_OP_divModInteger_dot___hash_selFP12_hash_m x1003 x3250 x3500)
     (Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_divModInteger_dot___hash_selFP12_hash_m z x3250 x3500) x1002
     (Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_divModInteger_dot___hash_selFP12_hash_m x1002 x3250) $! (addCs x1001 x3500))
     (Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_divModInteger_dot___hash_selFP14_hash_d :: OP_Tuple2 BinInt BinInt -> Cover -> ConstStore -> BinInt
d_OP_divModInteger_dot___hash_selFP14_hash_d x1 x3250 x3500 = case x1 of
     (OP_Tuple2 x2 x3) -> x2
     (Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_divModInteger_dot___hash_selFP14_hash_d x1002 x3250 x3500) (d_OP_divModInteger_dot___hash_selFP14_hash_d x1003 x3250 x3500)
     (Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_divModInteger_dot___hash_selFP14_hash_d z x3250 x3500) x1002
     (Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_divModInteger_dot___hash_selFP14_hash_d x1002 x3250) $! (addCs x1001 x3500))
     (Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_divModInteger_dot___hash_selFP15_hash_m :: OP_Tuple2 BinInt BinInt -> Cover -> ConstStore -> BinInt
d_OP_divModInteger_dot___hash_selFP15_hash_m x1 x3250 x3500 = case x1 of
     (OP_Tuple2 x2 x3) -> x3
     (Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_divModInteger_dot___hash_selFP15_hash_m x1002 x3250 x3500) (d_OP_divModInteger_dot___hash_selFP15_hash_m x1003 x3250 x3500)
     (Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_divModInteger_dot___hash_selFP15_hash_m z x3250 x3500) x1002
     (Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_divModInteger_dot___hash_selFP15_hash_m x1002 x3250) $! (addCs x1001 x3500))
     (Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_divModInteger_dot___hash_selFP17_hash_d :: OP_Tuple2 BinInt BinInt -> Cover -> ConstStore -> BinInt
d_OP_divModInteger_dot___hash_selFP17_hash_d x1 x3250 x3500 = case x1 of
     (OP_Tuple2 x2 x3) -> x2
     (Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_divModInteger_dot___hash_selFP17_hash_d x1002 x3250 x3500) (d_OP_divModInteger_dot___hash_selFP17_hash_d x1003 x3250 x3500)
     (Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_divModInteger_dot___hash_selFP17_hash_d z x3250 x3500) x1002
     (Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_divModInteger_dot___hash_selFP17_hash_d x1002 x3250) $! (addCs x1001 x3500))
     (Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_divModInteger_dot___hash_selFP18_hash_m :: OP_Tuple2 BinInt BinInt -> Cover -> ConstStore -> BinInt
d_OP_divModInteger_dot___hash_selFP18_hash_m x1 x3250 x3500 = case x1 of
     (OP_Tuple2 x2 x3) -> x3
     (Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_divModInteger_dot___hash_selFP18_hash_m x1002 x3250 x3500) (d_OP_divModInteger_dot___hash_selFP18_hash_m x1003 x3250 x3500)
     (Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_divModInteger_dot___hash_selFP18_hash_m z x3250 x3500) x1002
     (Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_divModInteger_dot___hash_selFP18_hash_m x1002 x3250) $! (addCs x1001 x3500))
     (Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_divInteger :: BinInt -> BinInt -> Cover -> ConstStore -> BinInt
d_C_divInteger x1 x2 x3250 x3500 = d_C_fst (d_C_divModInteger x1 x2 x3250 x3500) x3250 x3500

d_C_modInteger :: BinInt -> BinInt -> Cover -> ConstStore -> BinInt
d_C_modInteger x1 x2 x3250 x3500 = d_C_snd (d_C_divModInteger x1 x2 x3250 x3500) x3250 x3500

d_C_quotInteger :: BinInt -> BinInt -> Cover -> ConstStore -> BinInt
d_C_quotInteger x1 x2 x3250 x3500 = d_C_fst (d_C_quotRemInteger x1 x2 x3250 x3500) x3250 x3500

d_C_remInteger :: BinInt -> BinInt -> Cover -> ConstStore -> BinInt
d_C_remInteger x1 x2 x3250 x3500 = d_C_snd (d_C_quotRemInteger x1 x2 x3250 x3500) x3250 x3500

d_OP__casePT_1 x11 x1 x3250 x3500 = case x1 of
     Zero -> OP_Tuple2 Zero Zero
     (Pos x12) -> let
          x13 = d_C_quotRemNat x12 x11 x3250 x3500
          x14 = d_OP_divModInteger_dot___hash_selFP14_hash_d x13 x3250 x3500
          x15 = d_OP_divModInteger_dot___hash_selFP15_hash_m x13 x3250 x3500
           in (d_OP__casePT_0 x11 x14 x15 x3250 x3500)
     (Neg x18) -> let
          x19 = d_C_quotRemNat x18 x11 x3250 x3500
          x20 = d_OP_divModInteger_dot___hash_selFP17_hash_d x19 x3250 x3500
          x21 = d_OP_divModInteger_dot___hash_selFP18_hash_m x19 x3250 x3500
           in (OP_Tuple2 x20 (d_C_neg x21 x3250 x3500))
     (Choice_BinInt x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__casePT_1 x11 x1002 x3250 x3500) (d_OP__casePT_1 x11 x1003 x3250 x3500)
     (Choices_BinInt x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__casePT_1 x11 z x3250 x3500) x1002
     (Guard_BinInt x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__casePT_1 x11 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_BinInt x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__casePT_1 x11 x1 x3000 x3250 x3500 = case x1 of
     Zero -> OP_Tuple2 Zero Zero
     (Pos x12) -> let
          x2000 = x3000
           in (seq x2000 (let
               x13 = d_C_quotRemNat x12 x11 x3250 x3500
               x14 = d_OP_divModInteger_dot___hash_selFP14_hash_d x13 x3250 x3500
               x15 = d_OP_divModInteger_dot___hash_selFP15_hash_m x13 x3250 x3500
                in (nd_OP__casePT_0 x11 x14 x15 x2000 x3250 x3500)))
     (Neg x18) -> let
          x19 = d_C_quotRemNat x18 x11 x3250 x3500
          x20 = d_OP_divModInteger_dot___hash_selFP17_hash_d x19 x3250 x3500
          x21 = d_OP_divModInteger_dot___hash_selFP18_hash_m x19 x3250 x3500
           in (OP_Tuple2 x20 (d_C_neg x21 x3250 x3500))
     (Choice_BinInt x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__casePT_1 x11 x1002 x3000 x3250 x3500) (nd_OP__casePT_1 x11 x1003 x3000 x3250 x3500)
     (Choices_BinInt x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__casePT_1 x11 z x3000 x3250 x3500) x1002
     (Guard_BinInt x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__casePT_1 x11 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Fail_BinInt x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__casePT_0 x11 x14 x15 x3250 x3500 = case x15 of
     Zero -> OP_Tuple2 (d_C_neg x14 x3250 x3500) x15
     (Neg x16) -> OP_Tuple2 (d_C_neg (d_C_inc x14 x3250 x3500) x3250 x3500) (d_OP_minus_hash x15 (Pos x11) x3250 x3500)
     (Pos x17) -> OP_Tuple2 (d_C_neg (d_C_inc x14 x3250 x3500) x3250 x3500) (d_OP_minus_hash x15 (Pos x11) x3250 x3500)
     (Choice_BinInt x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__casePT_0 x11 x14 x1002 x3250 x3500) (d_OP__casePT_0 x11 x14 x1003 x3250 x3500)
     (Choices_BinInt x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__casePT_0 x11 x14 z x3250 x3500) x1002
     (Guard_BinInt x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__casePT_0 x11 x14 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_BinInt x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__casePT_0 x11 x14 x15 x3000 x3250 x3500 = case x15 of
     Zero -> OP_Tuple2 (d_C_neg x14 x3250 x3500) x15
     (Neg x16) -> OP_Tuple2 (d_C_neg (d_C_inc x14 x3250 x3500) x3250 x3500) (d_OP_minus_hash x15 (Pos x11) x3250 x3500)
     (Pos x17) -> OP_Tuple2 (d_C_neg (d_C_inc x14 x3250 x3500) x3250 x3500) (d_OP_minus_hash x15 (Pos x11) x3250 x3500)
     (Choice_BinInt x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__casePT_0 x11 x14 x1002 x3000 x3250 x3500) (nd_OP__casePT_0 x11 x14 x1003 x3000 x3250 x3500)
     (Choices_BinInt x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__casePT_0 x11 x14 z x3000 x3250 x3500) x1002
     (Guard_BinInt x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__casePT_0 x11 x14 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Fail_BinInt x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__casePT_3 x3 x1 x3250 x3500 = case x1 of
     Zero -> OP_Tuple2 Zero Zero
     (Pos x4) -> d_C_quotRemNat x4 x3 x3250 x3500
     (Neg x5) -> let
          x6 = d_C_quotRemNat x5 x3 x3250 x3500
          x7 = d_OP_divModInteger_dot___hash_selFP11_hash_d x6 x3250 x3500
          x8 = d_OP_divModInteger_dot___hash_selFP12_hash_m x6 x3250 x3500
           in (d_OP__casePT_2 x3 x7 x8 x3250 x3500)
     (Choice_BinInt x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__casePT_3 x3 x1002 x3250 x3500) (d_OP__casePT_3 x3 x1003 x3250 x3500)
     (Choices_BinInt x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__casePT_3 x3 z x3250 x3500) x1002
     (Guard_BinInt x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__casePT_3 x3 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_BinInt x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__casePT_3 x3 x1 x3000 x3250 x3500 = case x1 of
     Zero -> OP_Tuple2 Zero Zero
     (Pos x4) -> d_C_quotRemNat x4 x3 x3250 x3500
     (Neg x5) -> let
          x2000 = x3000
           in (seq x2000 (let
               x6 = d_C_quotRemNat x5 x3 x3250 x3500
               x7 = d_OP_divModInteger_dot___hash_selFP11_hash_d x6 x3250 x3500
               x8 = d_OP_divModInteger_dot___hash_selFP12_hash_m x6 x3250 x3500
                in (nd_OP__casePT_2 x3 x7 x8 x2000 x3250 x3500)))
     (Choice_BinInt x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__casePT_3 x3 x1002 x3000 x3250 x3500) (nd_OP__casePT_3 x3 x1003 x3000 x3250 x3500)
     (Choices_BinInt x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__casePT_3 x3 z x3000 x3250 x3500) x1002
     (Guard_BinInt x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__casePT_3 x3 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Fail_BinInt x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__casePT_2 x3 x7 x8 x3250 x3500 = case x8 of
     Zero -> OP_Tuple2 (d_C_neg x7 x3250 x3500) x8
     (Neg x9) -> OP_Tuple2 (d_C_neg (d_C_inc x7 x3250 x3500) x3250 x3500) (d_OP_minus_hash (Pos x3) x8 x3250 x3500)
     (Pos x10) -> OP_Tuple2 (d_C_neg (d_C_inc x7 x3250 x3500) x3250 x3500) (d_OP_minus_hash (Pos x3) x8 x3250 x3500)
     (Choice_BinInt x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__casePT_2 x3 x7 x1002 x3250 x3500) (d_OP__casePT_2 x3 x7 x1003 x3250 x3500)
     (Choices_BinInt x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__casePT_2 x3 x7 z x3250 x3500) x1002
     (Guard_BinInt x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__casePT_2 x3 x7 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_BinInt x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__casePT_2 x3 x7 x8 x3000 x3250 x3500 = case x8 of
     Zero -> OP_Tuple2 (d_C_neg x7 x3250 x3500) x8
     (Neg x9) -> OP_Tuple2 (d_C_neg (d_C_inc x7 x3250 x3500) x3250 x3500) (d_OP_minus_hash (Pos x3) x8 x3250 x3500)
     (Pos x10) -> OP_Tuple2 (d_C_neg (d_C_inc x7 x3250 x3500) x3250 x3500) (d_OP_minus_hash (Pos x3) x8 x3250 x3500)
     (Choice_BinInt x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__casePT_2 x3 x7 x1002 x3000 x3250 x3500) (nd_OP__casePT_2 x3 x7 x1003 x3000 x3250 x3500)
     (Choices_BinInt x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__casePT_2 x3 x7 z x3000 x3250 x3500) x1002
     (Guard_BinInt x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__casePT_2 x3 x7 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Fail_BinInt x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__casePT_4 x9 x1 x3250 x3500 = case x1 of
     Zero -> OP_Tuple2 Zero Zero
     (Pos x10) -> let
          x11 = d_C_quotRemNat x10 x9 x3250 x3500
          x12 = d_OP_quotRemInteger_dot___hash_selFP5_hash_d x11 x3250 x3500
          x13 = d_OP_quotRemInteger_dot___hash_selFP6_hash_m x11 x3250 x3500
           in (OP_Tuple2 (d_C_neg x12 x3250 x3500) x13)
     (Neg x14) -> let
          x15 = d_C_quotRemNat x14 x9 x3250 x3500
          x16 = d_OP_quotRemInteger_dot___hash_selFP8_hash_d x15 x3250 x3500
          x17 = d_OP_quotRemInteger_dot___hash_selFP9_hash_m x15 x3250 x3500
           in (OP_Tuple2 x16 (d_C_neg x17 x3250 x3500))
     (Choice_BinInt x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__casePT_4 x9 x1002 x3250 x3500) (d_OP__casePT_4 x9 x1003 x3250 x3500)
     (Choices_BinInt x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__casePT_4 x9 z x3250 x3500) x1002
     (Guard_BinInt x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__casePT_4 x9 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_BinInt x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__casePT_4 x9 x1 x3000 x3250 x3500 = case x1 of
     Zero -> OP_Tuple2 Zero Zero
     (Pos x10) -> let
          x11 = d_C_quotRemNat x10 x9 x3250 x3500
          x12 = d_OP_quotRemInteger_dot___hash_selFP5_hash_d x11 x3250 x3500
          x13 = d_OP_quotRemInteger_dot___hash_selFP6_hash_m x11 x3250 x3500
           in (OP_Tuple2 (d_C_neg x12 x3250 x3500) x13)
     (Neg x14) -> let
          x15 = d_C_quotRemNat x14 x9 x3250 x3500
          x16 = d_OP_quotRemInteger_dot___hash_selFP8_hash_d x15 x3250 x3500
          x17 = d_OP_quotRemInteger_dot___hash_selFP9_hash_m x15 x3250 x3500
           in (OP_Tuple2 x16 (d_C_neg x17 x3250 x3500))
     (Choice_BinInt x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__casePT_4 x9 x1002 x3000 x3250 x3500) (nd_OP__casePT_4 x9 x1003 x3000 x3250 x3500)
     (Choices_BinInt x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__casePT_4 x9 z x3000 x3250 x3500) x1002
     (Guard_BinInt x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__casePT_4 x9 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Fail_BinInt x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__casePT_5 x3 x1 x3250 x3500 = case x1 of
     Zero -> OP_Tuple2 Zero Zero
     (Pos x4) -> d_C_quotRemNat x4 x3 x3250 x3500
     (Neg x5) -> let
          x6 = d_C_quotRemNat x5 x3 x3250 x3500
          x7 = d_OP_quotRemInteger_dot___hash_selFP2_hash_d x6 x3250 x3500
          x8 = d_OP_quotRemInteger_dot___hash_selFP3_hash_m x6 x3250 x3500
           in (OP_Tuple2 (d_C_neg x7 x3250 x3500) (d_C_neg x8 x3250 x3500))
     (Choice_BinInt x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__casePT_5 x3 x1002 x3250 x3500) (d_OP__casePT_5 x3 x1003 x3250 x3500)
     (Choices_BinInt x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__casePT_5 x3 z x3250 x3500) x1002
     (Guard_BinInt x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__casePT_5 x3 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_BinInt x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__casePT_5 x3 x1 x3000 x3250 x3500 = case x1 of
     Zero -> OP_Tuple2 Zero Zero
     (Pos x4) -> d_C_quotRemNat x4 x3 x3250 x3500
     (Neg x5) -> let
          x6 = d_C_quotRemNat x5 x3 x3250 x3500
          x7 = d_OP_quotRemInteger_dot___hash_selFP2_hash_d x6 x3250 x3500
          x8 = d_OP_quotRemInteger_dot___hash_selFP3_hash_m x6 x3250 x3500
           in (OP_Tuple2 (d_C_neg x7 x3250 x3500) (d_C_neg x8 x3250 x3500))
     (Choice_BinInt x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__casePT_5 x3 x1002 x3000 x3250 x3500) (nd_OP__casePT_5 x3 x1003 x3000 x3250 x3500)
     (Choices_BinInt x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__casePT_5 x3 z x3000 x3250 x3500) x1002
     (Guard_BinInt x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__casePT_5 x3 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Fail_BinInt x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__casePT_6 x6 x2 x3250 x3500 = case x2 of
     Zero -> Zero
     (Pos x7) -> Neg (d_OP_star_caret x6 x7 x3250 x3500)
     (Neg x8) -> Pos (d_OP_star_caret x6 x8 x3250 x3500)
     (Choice_BinInt x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__casePT_6 x6 x1002 x3250 x3500) (d_OP__casePT_6 x6 x1003 x3250 x3500)
     (Choices_BinInt x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__casePT_6 x6 z x3250 x3500) x1002
     (Guard_BinInt x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__casePT_6 x6 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_BinInt x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__casePT_6 x6 x2 x3000 x3250 x3500 = case x2 of
     Zero -> Zero
     (Pos x7) -> Neg (d_OP_star_caret x6 x7 x3250 x3500)
     (Neg x8) -> Pos (d_OP_star_caret x6 x8 x3250 x3500)
     (Choice_BinInt x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__casePT_6 x6 x1002 x3000 x3250 x3500) (nd_OP__casePT_6 x6 x1003 x3000 x3250 x3500)
     (Choices_BinInt x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__casePT_6 x6 z x3000 x3250 x3500) x1002
     (Guard_BinInt x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__casePT_6 x6 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Fail_BinInt x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__casePT_7 x3 x2 x3250 x3500 = case x2 of
     Zero -> Zero
     (Pos x4) -> Pos (d_OP_star_caret x3 x4 x3250 x3500)
     (Neg x5) -> Neg (d_OP_star_caret x3 x5 x3250 x3500)
     (Choice_BinInt x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__casePT_7 x3 x1002 x3250 x3500) (d_OP__casePT_7 x3 x1003 x3250 x3500)
     (Choices_BinInt x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__casePT_7 x3 z x3250 x3500) x1002
     (Guard_BinInt x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__casePT_7 x3 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_BinInt x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__casePT_7 x3 x2 x3000 x3250 x3500 = case x2 of
     Zero -> Zero
     (Pos x4) -> Pos (d_OP_star_caret x3 x4 x3250 x3500)
     (Neg x5) -> Neg (d_OP_star_caret x3 x5 x3250 x3500)
     (Choice_BinInt x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__casePT_7 x3 x1002 x3000 x3250 x3500) (nd_OP__casePT_7 x3 x1003 x3000 x3250 x3500)
     (Choices_BinInt x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__casePT_7 x3 z x3000 x3250 x3500) x1002
     (Guard_BinInt x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__casePT_7 x3 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Fail_BinInt x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__casePT_8 x1 x6 x2 x3250 x3500 = case x2 of
     Zero -> x1
     (Pos x7) -> d_OP_minus_caret x7 x6 x3250 x3500
     (Neg x8) -> Neg (d_OP_plus_caret x6 x8 x3250 x3500)
     (Choice_BinInt x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__casePT_8 x1 x6 x1002 x3250 x3500) (d_OP__casePT_8 x1 x6 x1003 x3250 x3500)
     (Choices_BinInt x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__casePT_8 x1 x6 z x3250 x3500) x1002
     (Guard_BinInt x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__casePT_8 x1 x6 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_BinInt x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__casePT_8 x1 x6 x2 x3000 x3250 x3500 = case x2 of
     Zero -> x1
     (Pos x7) -> d_OP_minus_caret x7 x6 x3250 x3500
     (Neg x8) -> Neg (d_OP_plus_caret x6 x8 x3250 x3500)
     (Choice_BinInt x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__casePT_8 x1 x6 x1002 x3000 x3250 x3500) (nd_OP__casePT_8 x1 x6 x1003 x3000 x3250 x3500)
     (Choices_BinInt x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__casePT_8 x1 x6 z x3000 x3250 x3500) x1002
     (Guard_BinInt x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__casePT_8 x1 x6 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Fail_BinInt x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__casePT_9 x1 x3 x2 x3250 x3500 = case x2 of
     Zero -> x1
     (Pos x4) -> Pos (d_OP_plus_caret x3 x4 x3250 x3500)
     (Neg x5) -> d_OP_minus_caret x3 x5 x3250 x3500
     (Choice_BinInt x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__casePT_9 x1 x3 x1002 x3250 x3500) (d_OP__casePT_9 x1 x3 x1003 x3250 x3500)
     (Choices_BinInt x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__casePT_9 x1 x3 z x3250 x3500) x1002
     (Guard_BinInt x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__casePT_9 x1 x3 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_BinInt x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__casePT_9 x1 x3 x2 x3000 x3250 x3500 = case x2 of
     Zero -> x1
     (Pos x4) -> Pos (d_OP_plus_caret x3 x4 x3250 x3500)
     (Neg x5) -> d_OP_minus_caret x3 x5 x3250 x3500
     (Choice_BinInt x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__casePT_9 x1 x3 x1002 x3000 x3250 x3500) (nd_OP__casePT_9 x1 x3 x1003 x3000 x3250 x3500)
     (Choices_BinInt x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__casePT_9 x1 x3 z x3000 x3250 x3500) x1002
     (Guard_BinInt x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__casePT_9 x1 x3 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Fail_BinInt x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__casePT_10 x2 x3250 x3500 = case x2 of
     IHi -> Zero
     (O x3) -> Pos (d_C_pred (O x3) x3250 x3500)
     (I x4) -> Pos (O x4)
     (Choice_Nat  x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__casePT_10 x1002 x3250 x3500) (d_OP__casePT_10 x1003 x3250 x3500)
     (Choices_Nat  x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__casePT_10 z x3250 x3500) x1002
     (Guard_Nat  x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__casePT_10 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_Nat  x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__casePT_10 x2 x3000 x3250 x3500 = case x2 of
     IHi -> Zero
     (O x3) -> Pos (d_C_pred (O x3) x3250 x3500)
     (I x4) -> Pos (O x4)
     (Choice_Nat  x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__casePT_10 x1002 x3000 x3250 x3500) (nd_OP__casePT_10 x1003 x3000 x3250 x3500)
     (Choices_Nat  x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__casePT_10 z x3000 x3250 x3500) x1002
     (Guard_Nat  x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__casePT_10 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Fail_Nat  x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__casePT_11 x3 x3250 x3500 = case x3 of
     IHi -> Zero
     (O x4) -> Neg (d_C_pred (O x4) x3250 x3500)
     (I x5) -> Neg (O x5)
     (Choice_Nat  x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__casePT_11 x1002 x3250 x3500) (d_OP__casePT_11 x1003 x3250 x3500)
     (Choices_Nat  x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__casePT_11 z x3250 x3500) x1002
     (Guard_Nat  x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__casePT_11 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_Nat  x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__casePT_11 x3 x3000 x3250 x3500 = case x3 of
     IHi -> Zero
     (O x4) -> Neg (d_C_pred (O x4) x3250 x3500)
     (I x5) -> Neg (O x5)
     (Choice_Nat  x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__casePT_11 x1002 x3000 x3250 x3500) (nd_OP__casePT_11 x1003 x3000 x3250 x3500)
     (Choices_Nat  x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__casePT_11 z x3000 x3250 x3500) x1002
     (Guard_Nat  x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__casePT_11 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Fail_Nat  x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__casePT_12 x8 x2 x3250 x3500 = case x2 of
     Zero -> C_LT
     (Pos x9) -> C_LT
     (Neg x10) -> d_C_cmpNat x10 x8 x3250 x3500
     (Choice_BinInt x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__casePT_12 x8 x1002 x3250 x3500) (d_OP__casePT_12 x8 x1003 x3250 x3500)
     (Choices_BinInt x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__casePT_12 x8 z x3250 x3500) x1002
     (Guard_BinInt x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__casePT_12 x8 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_BinInt x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__casePT_12 x8 x2 x3000 x3250 x3500 = case x2 of
     Zero -> C_LT
     (Pos x9) -> C_LT
     (Neg x10) -> d_C_cmpNat x10 x8 x3250 x3500
     (Choice_BinInt x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__casePT_12 x8 x1002 x3000 x3250 x3500) (nd_OP__casePT_12 x8 x1003 x3000 x3250 x3500)
     (Choices_BinInt x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__casePT_12 x8 z x3000 x3250 x3500) x1002
     (Guard_BinInt x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__casePT_12 x8 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Fail_BinInt x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__casePT_13 x5 x2 x3250 x3500 = case x2 of
     Zero -> C_GT
     (Pos x6) -> d_C_cmpNat x5 x6 x3250 x3500
     (Neg x7) -> C_GT
     (Choice_BinInt x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__casePT_13 x5 x1002 x3250 x3500) (d_OP__casePT_13 x5 x1003 x3250 x3500)
     (Choices_BinInt x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__casePT_13 x5 z x3250 x3500) x1002
     (Guard_BinInt x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__casePT_13 x5 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_BinInt x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__casePT_13 x5 x2 x3000 x3250 x3500 = case x2 of
     Zero -> C_GT
     (Pos x6) -> d_C_cmpNat x5 x6 x3250 x3500
     (Neg x7) -> C_GT
     (Choice_BinInt x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__casePT_13 x5 x1002 x3000 x3250 x3500) (nd_OP__casePT_13 x5 x1003 x3000 x3250 x3500)
     (Choices_BinInt x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__casePT_13 x5 z x3000 x3250 x3500) x1002
     (Guard_BinInt x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__casePT_13 x5 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Fail_BinInt x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__casePT_14 x2 x3250 x3500 = case x2 of
     Zero -> C_EQ
     (Pos x3) -> C_LT
     (Neg x4) -> C_GT
     (Choice_BinInt x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__casePT_14 x1002 x3250 x3500) (d_OP__casePT_14 x1003 x3250 x3500)
     (Choices_BinInt x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__casePT_14 z x3250 x3500) x1002
     (Guard_BinInt x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__casePT_14 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_BinInt x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__casePT_14 x2 x3000 x3250 x3500 = case x2 of
     Zero -> C_EQ
     (Pos x3) -> C_LT
     (Neg x4) -> C_GT
     (Choice_BinInt x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__casePT_14 x1002 x3000 x3250 x3500) (nd_OP__casePT_14 x1003 x3000 x3250 x3500)
     (Choices_BinInt x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__casePT_14 z x3000 x3250 x3500) x1002
     (Guard_BinInt x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__casePT_14 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Fail_BinInt x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__casePT_23 x1 x2 x3 x3250 x3500 = case x3 of
     C_True -> OP_Tuple2 (Pos x1) Zero
     C_False -> d_OP__casePT_22 x1 x2 (d_OP_eq_eq x1 IHi x3250 x3500) x3250 x3500
     (Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__casePT_23 x1 x2 x1002 x3250 x3500) (d_OP__casePT_23 x1 x2 x1003 x3250 x3500)
     (Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__casePT_23 x1 x2 z x3250 x3500) x1002
     (Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__casePT_23 x1 x2 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__casePT_23 x1 x2 x3 x3000 x3250 x3500 = case x3 of
     C_True -> OP_Tuple2 (Pos x1) Zero
     C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__casePT_22 x1 x2 (d_OP_eq_eq x1 IHi x3250 x3500) x2000 x3250 x3500))
     (Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__casePT_23 x1 x2 x1002 x3000 x3250 x3500) (nd_OP__casePT_23 x1 x2 x1003 x3000 x3250 x3500)
     (Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__casePT_23 x1 x2 z x3000 x3250 x3500) x1002
     (Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__casePT_23 x1 x2 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__casePT_22 x1 x2 x3 x3250 x3500 = case x3 of
     C_True -> OP_Tuple2 Zero (Pos x2)
     C_False -> d_OP__casePT_21 x1 x2 (d_C_otherwise x3250 x3500) x3250 x3500
     (Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__casePT_22 x1 x2 x1002 x3250 x3500) (d_OP__casePT_22 x1 x2 x1003 x3250 x3500)
     (Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__casePT_22 x1 x2 z x3250 x3500) x1002
     (Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__casePT_22 x1 x2 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__casePT_22 x1 x2 x3 x3000 x3250 x3500 = case x3 of
     C_True -> OP_Tuple2 Zero (Pos x2)
     C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__casePT_21 x1 x2 (d_C_otherwise x3250 x3500) x2000 x3250 x3500))
     (Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__casePT_22 x1 x2 x1002 x3000 x3250 x3500) (nd_OP__casePT_22 x1 x2 x1003 x3000 x3250 x3500)
     (Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__casePT_22 x1 x2 z x3000 x3250 x3500) x1002
     (Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__casePT_22 x1 x2 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__casePT_21 x1 x2 x3 x3250 x3500 = case x3 of
     C_True -> d_OP__casePT_20 x1 x2 (d_C_cmpNat x1 x2 x3250 x3500) x3250 x3500
     C_False -> d_C_failed x3250 x3500
     (Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__casePT_21 x1 x2 x1002 x3250 x3500) (d_OP__casePT_21 x1 x2 x1003 x3250 x3500)
     (Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__casePT_21 x1 x2 z x3250 x3500) x1002
     (Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__casePT_21 x1 x2 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__casePT_21 x1 x2 x3 x3000 x3250 x3500 = case x3 of
     C_True -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__casePT_20 x1 x2 (d_C_cmpNat x1 x2 x3250 x3500) x2000 x3250 x3500))
     C_False -> d_C_failed x3250 x3500
     (Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__casePT_21 x1 x2 x1002 x3000 x3250 x3500) (nd_OP__casePT_21 x1 x2 x1003 x3000 x3250 x3500)
     (Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__casePT_21 x1 x2 z x3000 x3250 x3500) x1002
     (Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__casePT_21 x1 x2 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__casePT_20 x1 x2 x3 x3250 x3500 = case x3 of
     C_EQ -> OP_Tuple2 (Pos IHi) Zero
     C_LT -> OP_Tuple2 Zero (Pos x1)
     C_GT -> d_OP__casePT_19 x1 x2 (d_C_quotRemNat (d_C_div2 x1 x3250 x3500) x2 x3250 x3500) x3250 x3500
     (Choice_C_Ordering x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__casePT_20 x1 x2 x1002 x3250 x3500) (d_OP__casePT_20 x1 x2 x1003 x3250 x3500)
     (Choices_C_Ordering x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__casePT_20 x1 x2 z x3250 x3500) x1002
     (Guard_C_Ordering x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__casePT_20 x1 x2 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Ordering x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__casePT_20 x1 x2 x3 x3000 x3250 x3500 = case x3 of
     C_EQ -> OP_Tuple2 (Pos IHi) Zero
     C_LT -> OP_Tuple2 Zero (Pos x1)
     C_GT -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__casePT_19 x1 x2 (d_C_quotRemNat (d_C_div2 x1 x3250 x3500) x2 x3250 x3500) x2000 x3250 x3500))
     (Choice_C_Ordering x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__casePT_20 x1 x2 x1002 x3000 x3250 x3500) (nd_OP__casePT_20 x1 x2 x1003 x3000 x3250 x3500)
     (Choices_C_Ordering x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__casePT_20 x1 x2 z x3000 x3250 x3500) x1002
     (Guard_C_Ordering x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__casePT_20 x1 x2 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Fail_C_Ordering x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__casePT_19 x1 x2 x5 x3250 x3500 = case x5 of
     (OP_Tuple2 x3 x4) -> d_OP__casePT_18 x1 x2 x4 x3 x3250 x3500
     (Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__casePT_19 x1 x2 x1002 x3250 x3500) (d_OP__casePT_19 x1 x2 x1003 x3250 x3500)
     (Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__casePT_19 x1 x2 z x3250 x3500) x1002
     (Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__casePT_19 x1 x2 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__casePT_19 x1 x2 x5 x3000 x3250 x3500 = case x5 of
     (OP_Tuple2 x3 x4) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__casePT_18 x1 x2 x4 x3 x2000 x3250 x3500))
     (Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__casePT_19 x1 x2 x1002 x3000 x3250 x3500) (nd_OP__casePT_19 x1 x2 x1003 x3000 x3250 x3500)
     (Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__casePT_19 x1 x2 z x3000 x3250 x3500) x1002
     (Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__casePT_19 x1 x2 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__casePT_18 x1 x2 x4 x3 x3250 x3500 = case x3 of
     Zero -> OP_Tuple2 (Pos IHi) (d_OP_minus_caret x1 x2 x3250 x3500)
     (Pos x5) -> d_OP__casePT_17 x1 x2 x5 x4 x3250 x3500
     (Neg x12) -> d_C_failed x3250 x3500
     (Choice_BinInt x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__casePT_18 x1 x2 x4 x1002 x3250 x3500) (d_OP__casePT_18 x1 x2 x4 x1003 x3250 x3500)
     (Choices_BinInt x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__casePT_18 x1 x2 x4 z x3250 x3500) x1002
     (Guard_BinInt x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__casePT_18 x1 x2 x4 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_BinInt x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__casePT_18 x1 x2 x4 x3 x3000 x3250 x3500 = case x3 of
     Zero -> OP_Tuple2 (Pos IHi) (d_OP_minus_caret x1 x2 x3250 x3500)
     (Pos x5) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__casePT_17 x1 x2 x5 x4 x2000 x3250 x3500))
     (Neg x12) -> d_C_failed x3250 x3500
     (Choice_BinInt x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__casePT_18 x1 x2 x4 x1002 x3000 x3250 x3500) (nd_OP__casePT_18 x1 x2 x4 x1003 x3000 x3250 x3500)
     (Choices_BinInt x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__casePT_18 x1 x2 x4 z x3000 x3250 x3500) x1002
     (Guard_BinInt x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__casePT_18 x1 x2 x4 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Fail_BinInt x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__casePT_17 x1 x2 x5 x4 x3250 x3500 = case x4 of
     Zero -> OP_Tuple2 (Pos (O x5)) (d_C_mod2 x1 x3250 x3500)
     (Pos x6) -> d_OP__casePT_16 x1 x2 x5 x6 (d_C_quotRemNat (d_OP_quotRemNat_dot_shift_dot_104 x1 x6 x3250 x3500) x2 x3250 x3500) x3250 x3500
     (Neg x11) -> d_C_failed x3250 x3500
     (Choice_BinInt x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__casePT_17 x1 x2 x5 x1002 x3250 x3500) (d_OP__casePT_17 x1 x2 x5 x1003 x3250 x3500)
     (Choices_BinInt x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__casePT_17 x1 x2 x5 z x3250 x3500) x1002
     (Guard_BinInt x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__casePT_17 x1 x2 x5 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_BinInt x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__casePT_17 x1 x2 x5 x4 x3000 x3250 x3500 = case x4 of
     Zero -> OP_Tuple2 (Pos (O x5)) (d_C_mod2 x1 x3250 x3500)
     (Pos x6) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__casePT_16 x1 x2 x5 x6 (d_C_quotRemNat (d_OP_quotRemNat_dot_shift_dot_104 x1 x6 x3250 x3500) x2 x3250 x3500) x2000 x3250 x3500))
     (Neg x11) -> d_C_failed x3250 x3500
     (Choice_BinInt x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__casePT_17 x1 x2 x5 x1002 x3000 x3250 x3500) (nd_OP__casePT_17 x1 x2 x5 x1003 x3000 x3250 x3500)
     (Choices_BinInt x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__casePT_17 x1 x2 x5 z x3000 x3250 x3500) x1002
     (Guard_BinInt x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__casePT_17 x1 x2 x5 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Fail_BinInt x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__casePT_16 x1 x2 x5 x6 x9 x3250 x3500 = case x9 of
     (OP_Tuple2 x7 x8) -> d_OP__casePT_15 x5 x8 x7 x3250 x3500
     (Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__casePT_16 x1 x2 x5 x6 x1002 x3250 x3500) (d_OP__casePT_16 x1 x2 x5 x6 x1003 x3250 x3500)
     (Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__casePT_16 x1 x2 x5 x6 z x3250 x3500) x1002
     (Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__casePT_16 x1 x2 x5 x6 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__casePT_16 x1 x2 x5 x6 x9 x3000 x3250 x3500 = case x9 of
     (OP_Tuple2 x7 x8) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__casePT_15 x5 x8 x7 x2000 x3250 x3500))
     (Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__casePT_16 x1 x2 x5 x6 x1002 x3000 x3250 x3500) (nd_OP__casePT_16 x1 x2 x5 x6 x1003 x3000 x3250 x3500)
     (Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__casePT_16 x1 x2 x5 x6 z x3000 x3250 x3500) x1002
     (Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__casePT_16 x1 x2 x5 x6 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__casePT_15 x5 x8 x7 x3250 x3500 = case x7 of
     Zero -> OP_Tuple2 (Pos (O x5)) x8
     (Pos x9) -> OP_Tuple2 (Pos (d_OP_plus_caret (O x5) x9 x3250 x3500)) x8
     (Neg x10) -> d_C_failed x3250 x3500
     (Choice_BinInt x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__casePT_15 x5 x8 x1002 x3250 x3500) (d_OP__casePT_15 x5 x8 x1003 x3250 x3500)
     (Choices_BinInt x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__casePT_15 x5 x8 z x3250 x3500) x1002
     (Guard_BinInt x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__casePT_15 x5 x8 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_BinInt x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__casePT_15 x5 x8 x7 x3000 x3250 x3500 = case x7 of
     Zero -> OP_Tuple2 (Pos (O x5)) x8
     (Pos x9) -> OP_Tuple2 (Pos (d_OP_plus_caret (O x5) x9 x3250 x3500)) x8
     (Neg x10) -> d_C_failed x3250 x3500
     (Choice_BinInt x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__casePT_15 x5 x8 x1002 x3000 x3250 x3500) (nd_OP__casePT_15 x5 x8 x1003 x3000 x3250 x3500)
     (Choices_BinInt x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__casePT_15 x5 x8 z x3000 x3250 x3500) x1002
     (Guard_BinInt x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__casePT_15 x5 x8 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Fail_BinInt x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__casePT_24 x6 x2 x3250 x3500 = case x2 of
     IHi -> Pos (O x6)
     (O x7) -> d_C_inc (d_C_mult2 (d_OP_minus_caret x6 x7 x3250 x3500) x3250 x3500) x3250 x3500
     (I x8) -> d_C_mult2 (d_OP_minus_caret x6 x8 x3250 x3500) x3250 x3500
     (Choice_Nat  x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__casePT_24 x6 x1002 x3250 x3500) (d_OP__casePT_24 x6 x1003 x3250 x3500)
     (Choices_Nat  x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__casePT_24 x6 z x3250 x3500) x1002
     (Guard_Nat  x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__casePT_24 x6 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_Nat  x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__casePT_24 x6 x2 x3000 x3250 x3500 = case x2 of
     IHi -> Pos (O x6)
     (O x7) -> d_C_inc (d_C_mult2 (d_OP_minus_caret x6 x7 x3250 x3500) x3250 x3500) x3250 x3500
     (I x8) -> d_C_mult2 (d_OP_minus_caret x6 x8 x3250 x3500) x3250 x3500
     (Choice_Nat  x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__casePT_24 x6 x1002 x3000 x3250 x3500) (nd_OP__casePT_24 x6 x1003 x3000 x3250 x3500)
     (Choices_Nat  x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__casePT_24 x6 z x3000 x3250 x3500) x1002
     (Guard_Nat  x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__casePT_24 x6 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Fail_Nat  x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__casePT_25 x1 x3 x2 x3250 x3500 = case x2 of
     IHi -> Pos (d_C_pred x1 x3250 x3500)
     (O x4) -> d_C_mult2 (d_OP_minus_caret x3 x4 x3250 x3500) x3250 x3500
     (I x5) -> d_C_dec (d_C_mult2 (d_OP_minus_caret x3 x5 x3250 x3500) x3250 x3500) x3250 x3500
     (Choice_Nat  x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__casePT_25 x1 x3 x1002 x3250 x3500) (d_OP__casePT_25 x1 x3 x1003 x3250 x3500)
     (Choices_Nat  x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__casePT_25 x1 x3 z x3250 x3500) x1002
     (Guard_Nat  x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__casePT_25 x1 x3 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_Nat  x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__casePT_25 x1 x3 x2 x3000 x3250 x3500 = case x2 of
     IHi -> Pos (d_C_pred x1 x3250 x3500)
     (O x4) -> d_C_mult2 (d_OP_minus_caret x3 x4 x3250 x3500) x3250 x3500
     (I x5) -> d_C_dec (d_C_mult2 (d_OP_minus_caret x3 x5 x3250 x3500) x3250 x3500) x3250 x3500
     (Choice_Nat  x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__casePT_25 x1 x3 x1002 x3000 x3250 x3500) (nd_OP__casePT_25 x1 x3 x1003 x3000 x3250 x3500)
     (Choices_Nat  x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__casePT_25 x1 x3 z x3000 x3250 x3500) x1002
     (Guard_Nat  x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__casePT_25 x1 x3 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Fail_Nat  x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__casePT_26 x6 x2 x3250 x3500 = case x2 of
     IHi -> O (d_C_succ x6 x3250 x3500)
     (O x7) -> I (d_OP_plus_caret x6 x7 x3250 x3500)
     (I x8) -> O (d_OP_plus_caret (d_C_succ x6 x3250 x3500) x8 x3250 x3500)
     (Choice_Nat  x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__casePT_26 x6 x1002 x3250 x3500) (d_OP__casePT_26 x6 x1003 x3250 x3500)
     (Choices_Nat  x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__casePT_26 x6 z x3250 x3500) x1002
     (Guard_Nat  x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__casePT_26 x6 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_Nat  x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__casePT_26 x6 x2 x3000 x3250 x3500 = case x2 of
     IHi -> O (d_C_succ x6 x3250 x3500)
     (O x7) -> I (d_OP_plus_caret x6 x7 x3250 x3500)
     (I x8) -> O (d_OP_plus_caret (d_C_succ x6 x3250 x3500) x8 x3250 x3500)
     (Choice_Nat  x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__casePT_26 x6 x1002 x3000 x3250 x3500) (nd_OP__casePT_26 x6 x1003 x3000 x3250 x3500)
     (Choices_Nat  x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__casePT_26 x6 z x3000 x3250 x3500) x1002
     (Guard_Nat  x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__casePT_26 x6 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Fail_Nat  x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__casePT_27 x3 x2 x3250 x3500 = case x2 of
     IHi -> I x3
     (O x4) -> O (d_OP_plus_caret x3 x4 x3250 x3500)
     (I x5) -> I (d_OP_plus_caret x3 x5 x3250 x3500)
     (Choice_Nat  x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__casePT_27 x3 x1002 x3250 x3500) (d_OP__casePT_27 x3 x1003 x3250 x3500)
     (Choices_Nat  x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__casePT_27 x3 z x3250 x3500) x1002
     (Guard_Nat  x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__casePT_27 x3 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_Nat  x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__casePT_27 x3 x2 x3000 x3250 x3500 = case x2 of
     IHi -> I x3
     (O x4) -> O (d_OP_plus_caret x3 x4 x3250 x3500)
     (I x5) -> I (d_OP_plus_caret x3 x5 x3250 x3500)
     (Choice_Nat  x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__casePT_27 x3 x1002 x3000 x3250 x3500) (nd_OP__casePT_27 x3 x1003 x3000 x3250 x3500)
     (Choices_Nat  x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__casePT_27 x3 z x3000 x3250 x3500) x1002
     (Guard_Nat  x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__casePT_27 x3 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Fail_Nat  x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__casePT_28 x2 x3250 x3500 = case x2 of
     IHi -> IHi
     (O x3) -> I (d_C_pred x2 x3250 x3500)
     (I x4) -> I (O x4)
     (Choice_Nat  x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__casePT_28 x1002 x3250 x3500) (d_OP__casePT_28 x1003 x3250 x3500)
     (Choices_Nat  x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__casePT_28 z x3250 x3500) x1002
     (Guard_Nat  x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__casePT_28 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_Nat  x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__casePT_28 x2 x3000 x3250 x3500 = case x2 of
     IHi -> IHi
     (O x3) -> I (d_C_pred x2 x3250 x3500)
     (I x4) -> I (O x4)
     (Choice_Nat  x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__casePT_28 x1002 x3000 x3250 x3500) (nd_OP__casePT_28 x1003 x3000 x3250 x3500)
     (Choices_Nat  x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__casePT_28 z x3000 x3250 x3500) x1002
     (Guard_Nat  x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__casePT_28 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Fail_Nat  x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__casePT_30 x8 x2 x3250 x3500 = case x2 of
     IHi -> C_GT
     (O x9) -> d_OP__casePT_29 x8 x9 (d_C_cmpNat x8 x9 x3250 x3500) x3250 x3500
     (I x10) -> d_C_cmpNat x8 x10 x3250 x3500
     (Choice_Nat  x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__casePT_30 x8 x1002 x3250 x3500) (d_OP__casePT_30 x8 x1003 x3250 x3500)
     (Choices_Nat  x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__casePT_30 x8 z x3250 x3500) x1002
     (Guard_Nat  x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__casePT_30 x8 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_Nat  x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__casePT_30 x8 x2 x3000 x3250 x3500 = case x2 of
     IHi -> C_GT
     (O x9) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__casePT_29 x8 x9 (d_C_cmpNat x8 x9 x3250 x3500) x2000 x3250 x3500))
     (I x10) -> d_C_cmpNat x8 x10 x3250 x3500
     (Choice_Nat  x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__casePT_30 x8 x1002 x3000 x3250 x3500) (nd_OP__casePT_30 x8 x1003 x3000 x3250 x3500)
     (Choices_Nat  x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__casePT_30 x8 z x3000 x3250 x3500) x1002
     (Guard_Nat  x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__casePT_30 x8 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Fail_Nat  x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__casePT_29 x8 x9 x10 x3250 x3500 = case x10 of
     C_EQ -> C_GT
     C_LT -> C_LT
     C_GT -> C_GT
     (Choice_C_Ordering x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__casePT_29 x8 x9 x1002 x3250 x3500) (d_OP__casePT_29 x8 x9 x1003 x3250 x3500)
     (Choices_C_Ordering x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__casePT_29 x8 x9 z x3250 x3500) x1002
     (Guard_C_Ordering x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__casePT_29 x8 x9 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Ordering x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__casePT_29 x8 x9 x10 x3000 x3250 x3500 = case x10 of
     C_EQ -> C_GT
     C_LT -> C_LT
     C_GT -> C_GT
     (Choice_C_Ordering x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__casePT_29 x8 x9 x1002 x3000 x3250 x3500) (nd_OP__casePT_29 x8 x9 x1003 x3000 x3250 x3500)
     (Choices_C_Ordering x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__casePT_29 x8 x9 z x3000 x3250 x3500) x1002
     (Guard_C_Ordering x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__casePT_29 x8 x9 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Fail_C_Ordering x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__casePT_32 x5 x2 x3250 x3500 = case x2 of
     IHi -> C_GT
     (O x6) -> d_C_cmpNat x5 x6 x3250 x3500
     (I x7) -> d_OP__casePT_31 x5 x7 (d_C_cmpNat x5 x7 x3250 x3500) x3250 x3500
     (Choice_Nat  x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__casePT_32 x5 x1002 x3250 x3500) (d_OP__casePT_32 x5 x1003 x3250 x3500)
     (Choices_Nat  x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__casePT_32 x5 z x3250 x3500) x1002
     (Guard_Nat  x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__casePT_32 x5 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_Nat  x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__casePT_32 x5 x2 x3000 x3250 x3500 = case x2 of
     IHi -> C_GT
     (O x6) -> d_C_cmpNat x5 x6 x3250 x3500
     (I x7) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__casePT_31 x5 x7 (d_C_cmpNat x5 x7 x3250 x3500) x2000 x3250 x3500))
     (Choice_Nat  x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__casePT_32 x5 x1002 x3000 x3250 x3500) (nd_OP__casePT_32 x5 x1003 x3000 x3250 x3500)
     (Choices_Nat  x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__casePT_32 x5 z x3000 x3250 x3500) x1002
     (Guard_Nat  x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__casePT_32 x5 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Fail_Nat  x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__casePT_31 x5 x7 x8 x3250 x3500 = case x8 of
     C_EQ -> C_LT
     C_LT -> C_LT
     C_GT -> C_GT
     (Choice_C_Ordering x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__casePT_31 x5 x7 x1002 x3250 x3500) (d_OP__casePT_31 x5 x7 x1003 x3250 x3500)
     (Choices_C_Ordering x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__casePT_31 x5 x7 z x3250 x3500) x1002
     (Guard_C_Ordering x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__casePT_31 x5 x7 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Ordering x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__casePT_31 x5 x7 x8 x3000 x3250 x3500 = case x8 of
     C_EQ -> C_LT
     C_LT -> C_LT
     C_GT -> C_GT
     (Choice_C_Ordering x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__casePT_31 x5 x7 x1002 x3000 x3250 x3500) (nd_OP__casePT_31 x5 x7 x1003 x3000 x3250 x3500)
     (Choices_C_Ordering x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__casePT_31 x5 x7 z x3000 x3250 x3500) x1002
     (Guard_C_Ordering x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__casePT_31 x5 x7 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Fail_C_Ordering x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__casePT_33 x2 x3250 x3500 = case x2 of
     IHi -> C_EQ
     (O x3) -> C_LT
     (I x4) -> C_LT
     (Choice_Nat  x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__casePT_33 x1002 x3250 x3500) (d_OP__casePT_33 x1003 x3250 x3500)
     (Choices_Nat  x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__casePT_33 z x3250 x3500) x1002
     (Guard_Nat  x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__casePT_33 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_Nat  x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__casePT_33 x2 x3000 x3250 x3500 = case x2 of
     IHi -> C_EQ
     (O x3) -> C_LT
     (I x4) -> C_LT
     (Choice_Nat  x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__casePT_33 x1002 x3000 x3250 x3500) (nd_OP__casePT_33 x1003 x3000 x3250 x3500)
     (Choices_Nat  x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__casePT_33 z x3000 x3250 x3500) x1002
     (Guard_Nat  x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__casePT_33 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Fail_Nat  x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo
