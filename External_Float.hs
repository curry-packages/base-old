external_d_C_prim_Float_plus :: Curry_Prelude.C_Float -> Curry_Prelude.C_Float -> Cover -> ConstStore -> Curry_Prelude.C_Float
external_d_C_prim_Float_plus y x _ _ =
  toCurry ((fromCurry x + fromCurry y) :: Float)

external_d_C_prim_Float_minus :: Curry_Prelude.C_Float -> Curry_Prelude.C_Float -> Cover -> ConstStore -> Curry_Prelude.C_Float
external_d_C_prim_Float_minus y x _ _ =
  toCurry ((fromCurry x - fromCurry y) :: Float)

external_d_C_prim_Float_times :: Curry_Prelude.C_Float -> Curry_Prelude.C_Float -> Cover -> ConstStore -> Curry_Prelude.C_Float
external_d_C_prim_Float_times y x _ _ =
  toCurry ((fromCurry x * fromCurry y) :: Float)

external_d_C_prim_Float_div :: Curry_Prelude.C_Float -> Curry_Prelude.C_Float -> Cover -> ConstStore -> Curry_Prelude.C_Float
external_d_C_prim_Float_div y x _ _ =
  toCurry ((fromCurry x / fromCurry y) :: Float)

external_d_C_prim_i2f :: Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.C_Float
external_d_C_prim_i2f x _ _ = toCurry (fromInteger (fromCurry x) :: Float)

external_d_C_prim_truncate :: Curry_Prelude.C_Float -> Cover -> ConstStore -> Curry_Prelude.C_Int
external_d_C_prim_truncate x _ _ = toCurry (truncate (fromCurry x :: Float) :: Int)

external_d_C_prim_round :: Curry_Prelude.C_Float -> Cover -> ConstStore -> Curry_Prelude.C_Int
external_d_C_prim_round x _ _ = toCurry (round (fromCurry x :: Float) :: Int)

external_d_C_prim_sqrt :: Curry_Prelude.C_Float -> Cover -> ConstStore -> Curry_Prelude.C_Float
external_d_C_prim_sqrt x _ _ = toCurry (sqrt (fromCurry x :: Float))

external_d_C_prim_log :: Curry_Prelude.C_Float -> Cover -> ConstStore -> Curry_Prelude.C_Float
external_d_C_prim_log x _ _ = toCurry (log (fromCurry x :: Float))

external_d_C_prim_exp :: Curry_Prelude.C_Float -> Cover -> ConstStore -> Curry_Prelude.C_Float
external_d_C_prim_exp x _ _ = toCurry (exp (fromCurry x :: Float))

external_d_C_prim_sin :: Curry_Prelude.C_Float -> Cover -> ConstStore -> Curry_Prelude.C_Float
external_d_C_prim_sin x _ _ = toCurry (sin (fromCurry x :: Float))

external_d_C_prim_cos :: Curry_Prelude.C_Float -> Cover -> ConstStore -> Curry_Prelude.C_Float
external_d_C_prim_cos x _ _ = toCurry (cos (fromCurry x :: Float))

external_d_C_prim_tan :: Curry_Prelude.C_Float -> Cover -> ConstStore -> Curry_Prelude.C_Float
external_d_C_prim_tan x _ _ = toCurry (tan (fromCurry x :: Float))

external_d_C_prim_atan :: Curry_Prelude.C_Float -> Cover -> ConstStore -> Curry_Prelude.C_Float
external_d_C_prim_atan x _ _ = toCurry (atan (fromCurry x :: Float))

