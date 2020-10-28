{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-} -- ?
module LazyShader.Shader where

import LazyShader.Has
import LazyShader.Type
import LazyShader.Operators


data Shader a where
    Var :: (Typed a) => String -> Shader a
    Val :: (Typed a) => a -> Shader a
    Op1 :: (Typed a, Typed b)
        => Op1 a b -> Shader a -> Shader b
    Op2 :: (Typed a, Typed b, Typed c)
        => Op2 a b c -> Shader a -> Shader b -> Shader c
    Op3 :: (Typed a, Typed b, Typed c, Typed d)
        => Op3 a b c d -> Shader a -> Shader b -> Shader c -> Shader d
    Op4 :: (Typed a, Typed b, Typed c, Typed d, Typed e)
        => Op4 a b c d e -> Shader a -> Shader b -> Shader c -> Shader d -> Shader e


instance (GenType b a, HasX a b) => HasX (Shader a) (Shader b) where
    getX = Op1 X

instance (GenType b a, HasY a b) => HasY (Shader a) (Shader b) where
    getY = Op1 Y

instance (GenType b a, HasZ a b) => HasZ (Shader a) (Shader b) where
    getZ = Op1 Z

instance (GenType b a, HasW a b) => HasW (Shader a) (Shader b) where
    getW = Op1 W


instance Eq a => Eq (Shader a) where
    Var a == Var b = a == b
    Val a == Val b = a == b
    _ == _ = False -- TODO: implement the rest

instance (Typed a, Num a, Eq a) => Num (Shader a) where
    Val a + Val b = Val (a + b)
    Val 0 + b = b
    a + Val 0 = a
    a + b = Op2 Add a b

    Val a - Val b = Val (a - b)
    Val 0 - b = -b
    a - Val 0 = a
    Val 1 - Op2 Step a b = Op2 Step b a
    a - b = Op2 Sub a b

    Val a * Val b = Val (a * b)
    Val 1 * b = b
    a * Val 1 = a
    a * b = Op2 Mul a b

    abs (Val x) = Val (abs x)
    abs x = Op1 Abs x

    signum (Val x) = Val (signum x)
    signum x = Op1 Sign x

    fromInteger = Val . fromInteger

instance (Typed a, Fractional a, Eq a) => Fractional (Shader a) where
    Val a / Val b = Val (a / b)
    a / Val 1 = a
    a / b = Op2 Div a b

    fromRational = Val . fromRational

instance (GenType Float a, Floating a, Eq a) => Floating (Shader a) where
    pi    = Val pi
    sin   = mapSG (sin :: Float -> Float) Sin
    cos   = mapSG (cos :: Float -> Float) Cos
    tan   = mapSG (tan :: Float -> Float) Tan
    log   = mapSG (log :: Float -> Float) Log
    exp   = mapSG (exp :: Float -> Float) Exp
    asin  = mapSG (asin :: Float -> Float) Asin
    acos  = mapSG (acos :: Float -> Float) Acos
    atan  = mapSG (atan :: Float -> Float) Atan
    sinh  = mapSG (sinh :: Float -> Float) Sinh
    cosh  = mapSG (cosh :: Float -> Float) Cosh
    asinh = mapSG (asinh :: Float -> Float) Asinh
    acosh = mapSG (acosh :: Float -> Float) Acosh
    atanh = mapSG (atanh :: Float -> Float) Atanh


-- Nested Shader types, for example in 'Vec2 a' (= 'V2 (Shader a)'), cause
-- problems with polymorphisms, which I haven't figured how to solve yet. This
-- function helps anyhow to apply operations on all GenType's.
mapSG :: GenType t a => (t -> t) -> Op1 a a -> Shader a -> Shader a
mapSG f _ (Val (x :: a)) = Val $ mapG f x
mapSG _ op s = Op1 op s
