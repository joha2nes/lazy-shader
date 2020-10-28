{-# language FlexibleContexts #-}
{-# language AllowAmbiguousTypes #-}
{-# language ScopedTypeVariables #-}
module LazyShader.Functions where

import Prelude hiding (min, max)
import qualified Prelude as P

import Linear hiding (_xy, _x, _y)

import LazyShader.Shader
import LazyShader.Has
import LazyShader.Type
import LazyShader.Operators

-- op1

tanh :: GenType Float a => Shader a -> Shader a
tanh = mapSG (P.tanh :: Float -> Float) Tanh 

exp2 :: GenType Float a => Shader a -> Shader a
exp2 = Op1 Exp2

log2 :: GenType Float a => Shader a -> Shader a
log2 = Op1 Log2

sqrt :: GenType Float a => Shader a -> Shader a
sqrt = mapSG (P.sqrt :: Float -> Float) Sqrt

inversesqrt :: GenType Float a => Shader a -> Shader a
inversesqrt = Op1 Inversesqrt

sign :: (Typed a, Num a) => Shader a -> Shader a
sign = Op1 Sign

floor :: GenType Float a => Shader a -> Shader a
floor = mapSG (fromIntegral . P.floor :: Float -> Float) Floor

trunc :: GenType Float a => Shader a -> Shader a
trunc = mapSG (fromIntegral . truncate :: Float -> Float) Trunc

round :: GenType Float a => Shader a -> Shader a
round = mapSG (fromIntegral . P.round :: Float -> Float) Round

roundEven :: GenType Float a => Shader a -> Shader a
roundEven = Op1 RoundEven

ceil :: GenType Float a => Shader a -> Shader a
ceil = mapSG (fromIntegral . P.ceiling :: Float -> Float) Ceil

fract :: GenType Float a => Shader a -> Shader a
fract = mapSG (snd . properFraction :: Float -> Float) Fract -- or 'fst'?

isnan :: (GenType Float a, GenType Bool b) => Shader a -> Shader b
isnan = Op1 Isnan

isinf :: (GenType Float a, GenType Bool b) => Shader a -> Shader b
isinf = Op1 Isinf

length :: GenType Float a => Shader a -> Shader Float
length = Op1 Length

normalize :: GenType Float a => Shader a -> Shader a
normalize = Op1 Normalize

radians :: GenType Float a => Shader a -> Shader a
radians = mapSG (\(deg :: Float) -> pi * deg / 180) Radians

degrees :: GenType Float a => Shader a -> Shader a
degrees = mapSG (\(rad :: Float) -> rad * 180 / pi) Radians

_x :: (GenType t a, HasX a t) => Shader a -> Shader t
_x = Op1 X

_y :: (GenType t a, HasY a t) => Shader a -> Shader t
_y = Op1 Y

_z :: (GenType t a, HasZ a t) => Shader a -> Shader t
_z = Op1 Z

_w :: (GenType t a, HasW a t) => Shader a -> Shader t
_w = Op1 W

_xy :: (GenType t a, HasX a t, HasY a t) => Shader a -> Shader (V2 t)
_xy = Op1 XY

-- op2

mod :: GenType Float a => Shader a -> Shader a -> Shader a
mod = Op2 Mod

mod' :: GenType Float a => Shader a -> Shader Float -> Shader a
mod' = Op2 Mod'

min :: Typed a => Shader a -> Shader a -> Shader a
min = Op2 Min

min' :: (GenType t a, Num t, Ord t, Typed t) => Shader a -> Shader t -> Shader a
min' = Op2 Min'

max :: Typed a => Shader a -> Shader a -> Shader a
max = Op2 Max

max' :: (GenType t a, Num t, Ord t, Typed t) => Shader a -> Shader t -> Shader a
max' = Op2 Max'

step :: GenType Float a => Shader a -> Shader a -> Shader a
step = Op2 Step

step' :: GenType Float a => Shader Float -> Shader a -> Shader a
step' = Op2 Step'

pow :: GenType Float a => Shader a -> Shader a -> Shader a
pow = Op2 Pow

distance :: GenType Float a => Shader a -> Shader a -> Shader Float
distance = Op2 Distance

dot :: GenType Float a => Shader a -> Shader a -> Shader Float
dot = Op2 Dot

cross :: Shader (V3 Float) -> Shader (V3 Float) -> Shader (V3 Float)
cross = Op2 Cross

reflect :: GenType Float a => Shader a -> Shader a -> Shader a
reflect = Op2 Reflect

atan2 :: GenType Float a => Shader a -> Shader a -> Shader a
atan2 = Op2 Atan2

-- op3

clamp :: (GenType t a, Num t, Ord t) => Shader a -> Shader a -> Shader a -> Shader a
clamp = Op3 Clamp

clamp' :: (GenType t a, Typed t, Num t, Ord t) => Shader a -> Shader t -> Shader t -> Shader a
clamp' = Op3 Clamp'

mix :: GenType Float a => Shader a -> Shader a -> Shader a -> Shader a
mix = Op3 Mix

mix' :: GenType Float a => Shader a -> Shader a -> Shader Float -> Shader a
mix' = Op3 Mix'

mixB :: (GenType Float a, GenType Bool b) => Shader a -> Shader a -> Shader b -> Shader a
mixB = Op3 MixB

smoothstep :: GenType Float a => Shader a -> Shader a -> Shader a -> Shader a
smoothstep = Op3 Smoothstep

smoothstep' :: GenType Float a => Shader Float -> Shader Float -> Shader a -> Shader a
smoothstep' = Op3 Smoothstep'

fma :: GenType Float a => Shader a -> Shader a -> Shader a -> Shader a
fma = Op3 Fma

faceforward :: GenType Float a => Shader a -> Shader a -> Shader a -> Shader a
faceforward = Op3 Faceforward

refract :: GenType Float a => Shader a -> Shader a -> Shader Float -> Shader a
refract = Op3 Refract

--

vec2 :: Primitive a => Shader a -> Shader a -> Shader (V2 a)
vec2 (Val x) (Val y) = Val (V2 x y)
vec2 x y = Op2 MkVec2 x y

vec3 :: Primitive a => Shader a -> Shader a -> Shader a -> Shader (V3 a)
vec3 (Val x) (Val y) (Val z) = Val (V3 x y z)
vec3 x y z = Op3 MkVec3 x y z

vec4 :: Primitive a => Shader a -> Shader a -> Shader a -> Shader a -> Shader (V4 a)
vec4 (Val x) (Val y) (Val z) (Val w) = Val (V4 x y z w)
vec4 x y z w = Op4 MkVec4 x y z w

true :: GenType Bool a => Shader a
true = Val (pureG True)

false :: GenType Bool a => Shader a
false = Val (pureG False)

fragCoord :: Shader (V4 Float)
fragCoord = Var "gl_FragCoord"

time :: Shader Float
time = Var "u_time"

resolution :: Shader (V2 Float)
resolution = Var "u_resolution"

uv :: Shader (V2 Float)
uv = _xy fragCoord / resolution

uvX :: Shader (V2 Float)
uvX =
    let x = _x uv
        y = _y uv * (_y resolution / _x resolution)
    in vec2 x y 

uvY :: Shader (V2 Float)
uvY =
    let x = _x uv * (_x resolution / _y resolution)
        y = _y uv
    in vec2 x y 
