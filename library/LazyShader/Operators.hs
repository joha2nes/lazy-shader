{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
module LazyShader.Operators where

import Linear (V2, V3, V4)

import LazyShader.Type
import LazyShader.Has

data Op1 a b where
    -- Floating
    Sin :: GenType Float a => Op1 a a
    Cos :: GenType Float a => Op1 a a
    Tan :: GenType Float a => Op1 a a
    Log :: GenType Float a => Op1 a a
    Exp :: GenType Float a => Op1 a a
    Asin :: GenType Float a => Op1 a a
    Acos :: GenType Float a => Op1 a a
    Atan :: GenType Float a => Op1 a a
    Sinh :: GenType Float a => Op1 a a
    Cosh :: GenType Float a => Op1 a a
    Tanh :: GenType Float a => Op1 a a
    Asinh :: GenType Float a => Op1 a a
    Acosh :: GenType Float a => Op1 a a
    Atanh :: GenType Float a => Op1 a a
    Exp2 :: GenType Float a => Op1 a a
    Log2 :: GenType Float a => Op1 a a
    Sqrt :: GenType Float a => Op1 a a
    Inversesqrt :: GenType Float a => Op1 a a
    Abs :: Num a => Op1 a a
    Sign :: Num a => Op1 a a
    Floor :: GenType Float a => Op1 a a
    Trunc :: GenType Float a => Op1 a a
    Round :: GenType Float a => Op1 a a
    RoundEven :: GenType Float a => Op1 a a
    Ceil :: GenType Float a => Op1 a a
    Fract :: GenType Float a => Op1 a a
    Isnan :: (GenType Float a, GenType Bool b) => Op1 a b
    Isinf :: (GenType Float a, GenType Bool b) => Op1 a b
    Length :: GenType Float a => Op1 a Float
    Normalize :: GenType Float a => Op1 a a
    Radians :: GenType Float a => Op1 a a
    Degrees :: GenType Float a => Op1 a a
    -- Modf :: GenType Float a => Op2 a (a, a) -- genFType modf(genFType x, out genFType i)
    -- genIType floatBitsToInt(highp genFType value)genUType floatBitsToUint(highp genFTypevalue)
    -- genFType intBitsToFloat(highp genIType value)genFType uintBitsToFloat(highp genUTypevalue)
    -- genFType frexp(highp genFType x, out highpgenIType exp)

    -- Swizzles
    X :: (GenType t a, HasX a t) => Op1 a t
    Y :: (GenType t a, HasY a t) => Op1 a t
    Z :: (GenType t a, HasZ a t) => Op1 a t
    W :: (GenType t a, HasW a t) => Op1 a t
    XY :: (GenType t a, HasX a t, HasY a t) => Op1 a (V2 t)
    -- XYZ :: Op1 a b

data Op2 a b c where
    -- Num
    Add :: Num a => Op2 a a a
    Sub :: Num a => Op2 a a a
    Mul :: Num a => Op2 a a a
    -- Fractional
    Div :: Fractional a => Op2 a a a

    Mod :: Op2 a a a -- GenType Float a => Op2 a a a
    Mod' :: GenType Float a => Op2 a Float a
    Min :: Op2 a a a -- GenType t a => Op2 a a a
    Min' :: GenType t a => Op2 a t a
    Max :: Op2 a a a -- GenType t a => Op2 a a a
    Max' :: GenType t a => Op2 a t a
    Step :: GenType Float a => Op2 a a a 
    Step' :: GenType Float a => Op2 Float a a 
    Pow :: GenType Float a => Op2 a a a
    Distance :: GenType Float a => Op2 a a Float
    Dot :: GenType Float a => Op2 a a Float
    Cross :: Op2 (V3 Float) (V3 Float) (V3 Float)
    Reflect :: GenType Float a => Op2 a a a 
    Atan2 :: GenType Float a => Op2 a a a
    -- genFType ldexp(highp genFType x, highpgenIType exp)
    MkVec2 :: Primitive a => Op2 a a (V2 a)

data Op3 a b c d where
    Clamp :: Op3 a a a a -- GenType t a => Op3 a a a a
    Clamp' :: GenType t a => Op3 a t t a
    Mix :: GenType Float a => Op3 a a a a
    Mix' :: GenType Float a => Op3 a a Float a
    MixB :: (GenType Float a, GenType Bool b) => Op3 a a b a
    Smoothstep :: GenType Float a => Op3 a a a a
    Smoothstep' :: GenType Float a => Op3 Float Float a a
    Fma :: GenType Float a => Op3 a a a a
    Faceforward :: GenType Float a => Op3 a a a a
    Refract :: GenType Float a => Op3 a a Float a
    MkVec3 :: Primitive a => Op3 a a a (V3 a)

data Op4 a b c d e where
    MkVec4 :: Primitive a => Op4 a a a a (V4 a)
