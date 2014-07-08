------------------------------------------------------------------------------
--- A collection of operations on floating point numbers.
------------------------------------------------------------------------------

module Float(pi,(+.),(-.),(*.),(/.),(^.),i2f,truncate,round,recip,sqrt,log
             ,logBase, exp,sin,cos,tan,asin,acos,atan,sinh,cosh,tanh
            ,asinh,acosh,atanh) where

-- The operator declarations are similar to the standard arithmetic operators.

infixr 8 ^.
infixl 7 *., /.
infixl 6 +., -.


--- The number pi.
pi :: Float
pi = 3.141592653589793238

--- Addition on floats.
(+.)   :: Float -> Float -> Float
x +. y = (prim_Float_plus $# y) $# x

prim_Float_plus :: Float -> Float -> Float
prim_Float_plus external

--- Subtraction on floats.
(-.)   :: Float -> Float -> Float
x -. y = (prim_Float_minus $# y) $# x

prim_Float_minus :: Float -> Float -> Float
prim_Float_minus external

--- Multiplication on floats.
(*.)   :: Float -> Float -> Float
x *. y = (prim_Float_times $# y) $# x

prim_Float_times :: Float -> Float -> Float
prim_Float_times external

--- Division on floats.
(/.)   :: Float -> Float -> Float
x /. y = (prim_Float_div $# y) $# x

prim_Float_div :: Float -> Float -> Float
prim_Float_div external

--- The value of `a ^. b` is `a` raised to the power of `b`.
--- Executes in `O(log b)` steps.
---
--- @param a - The base.
--- @param b - The exponent.
--- @return `a` raised to the power of `b`.

(^.) :: Float -> Int -> Float
a ^. b | b < 0     = 1 /. a ^. (b * (-1))
       | otherwise = powaux 1.0 a b
  where
    powaux n x y = if y == 0 then n
                   else powaux (n *. if (y `mod` 2 == 1) then x else 1.0)
                               (x *. x)
                               (y `div` 2)

--- Conversion function from integers to floats.
i2f    :: Int -> Float
i2f x = prim_i2f $# x

prim_i2f :: Int -> Float
prim_i2f external

--- Conversion function from floats to integers.
--- The result is the closest integer between the argument and 0.
truncate :: Float -> Int
truncate x = prim_truncate $# x

prim_truncate :: Float -> Int
prim_truncate external

--- Conversion function from floats to integers.
--- The result is the nearest integer to the argument.
--- If the argument is equidistant between two integers,
--- it is rounded to the closest even integer value.

round :: Float -> Int
round x = prim_round $# x

prim_round :: Float -> Int
prim_round external

--- Reciprocal

recip :: Float -> Float
recip x = 1.0 /. x

--- Square root.

sqrt :: Float -> Float
sqrt x = prim_sqrt $# x

prim_sqrt :: Float -> Float
prim_sqrt external

--- Natural logarithm.

log :: Float -> Float
log x = prim_log $# x

prim_log :: Float -> Float
prim_log external

--- Logarithm to arbitrary Base.

logBase :: Float -> Float -> Float
logBase x y = log y /. log x

--- Natural exponent.
exp :: Float -> Float
exp x = prim_exp $# x

prim_exp :: Float -> Float
prim_exp external

--- Sine.
sin :: Float -> Float
sin x = prim_sin $# x

prim_sin :: Float -> Float
prim_sin external

--- Cosine.
cos :: Float -> Float
cos x = prim_cos $# x

prim_cos :: Float -> Float
prim_cos external

--- Tangent.
tan :: Float -> Float
tan x = prim_tan $# x

prim_tan :: Float -> Float
prim_tan external

--- Arc sine.
asin :: Float -> Float
asin x = prim_asin $# x

prim_asin :: Float -> Float
prim_asin external

-- Arc cosine.
acos :: Float -> Float
acos x = prim_acos $# x

prim_acos :: Float -> Float
prim_acos external

--- Arc tangent.
atan :: Float -> Float
atan x = prim_atan $# x

prim_atan :: Float -> Float
prim_atan external

--- Hyperbolic sine.
sinh :: Float -> Float
sinh x = prim_sinh $# x

prim_sinh :: Float -> Float
prim_sinh external

-- Hyperbolic cosine.
cosh :: Float -> Float
cosh x = prim_cosh $# x

prim_cosh :: Float -> Float
prim_cosh external

--- Hyperbolic tangent.
tanh :: Float -> Float
tanh x = prim_tanh $# x

prim_tanh :: Float -> Float
prim_tanh external

--- Hyperbolic Arc sine.
asinh :: Float -> Float
asinh x = prim_asinh $# x

prim_asinh :: Float -> Float
prim_asinh external

-- Hyperbolic Arc cosine.
acosh :: Float -> Float
acosh x = prim_acosh $# x

prim_acosh :: Float -> Float
prim_acosh external

--- Hyperbolic Arc tangent.
atanh :: Float -> Float
atanh x = prim_atanh $# x

prim_atanh :: Float -> Float
prim_atanh external
