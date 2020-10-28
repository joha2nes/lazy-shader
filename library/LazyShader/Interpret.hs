{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
module LazyShader.Interpret (
    interpret
) where

import Data.Char (toLower)

import Linear

import LazyShader.Shader
import LazyShader.Operators
import LazyShader.Type

interpret :: Shader a -> String
interpret = \case
    Var s -> s
    Val (a :: a) -> case (typeof :: Type a) of
        Prim -> showPrim a
        Vec2 p -> let V2 x y = a in vecPrefix p <> "vec2(" <> showPrim x <> "," <> showPrim y <> ")"
        Vec3 p -> let V3 x y z = a in vecPrefix p <> "vec3(" <> showPrim x <> "," <> showPrim y <> "," <> showPrim z <> ")"
        Vec4 p -> let V4 x y z w = a in vecPrefix p <> "vec4(" <> showPrim x <> "," <> showPrim y <> "," <> showPrim z <> "," <> showPrim w <> ")"
    Op1 op x -> formatOp1 op (interpret x)
    Op2 op x y -> formatOp2 op (interpret x) (interpret y)
    Op3 op x y z -> formatOp3 op (interpret x) (interpret y) (interpret z)
    Op4 op x y z w -> formatOp4 op (interpret x) (interpret y) (interpret z) (interpret w)

showPrim :: Primitive a => a -> String
showPrim (a :: t) = case (primof :: PrimType t) of
    Bool -> toLower <$> show a
    Int -> show a
    Word -> show a
    Float -> show a

vecPrefix :: PrimType a -> String
vecPrefix Bool = "b"
vecPrefix Int = "i"
vecPrefix Word = "u"
vecPrefix Float = ""

formatOp1 :: Op1 a b -> String -> String
formatOp1 = \case
    Sin -> fn "sin"
    Cos -> fn "cos"
    Tan -> fn "tan"
    Log -> fn "log"
    Exp -> fn "exp"
    Asin -> fn "asin"
    Acos -> fn "acos"
    Atan -> fn "atan"
    Sinh -> fn "sinh"
    Cosh -> fn "cosh"
    Tanh -> fn "tanh"
    Asinh -> fn "asinh"
    Acosh -> fn "acosh"
    Atanh -> fn "atanh"
    Exp2 -> fn "exp2"
    Log2 -> fn "log2"
    Sqrt -> fn "sqrt"
    Inversesqrt -> fn "inversesqrt"
    Abs -> fn "abs"
    Sign -> fn "sign"
    Floor -> fn "floor"
    Trunc -> fn "trunc"
    Round -> fn "round"
    RoundEven -> fn "roundEven"
    Ceil -> fn "ceil"
    Fract -> fn "fract"
    Isnan -> fn "isnan"
    Isinf -> fn "isinf"
    Length -> fn "length"
    Normalize -> fn "normalize"
    Radians -> fn "radians"
    Degrees -> fn "degrees"
    X -> acc "x"
    Y -> acc "y"
    Z -> acc "z"
    W -> acc "w"
    XY -> acc "xy"
    where
        fn s = \a -> s <> "(" <> a <> ")"
        acc s = \a -> a <> "." <> s

formatOp2 :: Op2 a b c -> String -> String -> String
formatOp2 (op :: Op2 a b c) = case op of
    Add -> binop "+"
    Sub -> binop "-"
    Mul -> binop "*"
    Div -> binop "/"
    Mod -> fn "mod"
    Mod' -> fn "mod"
    Min -> fn "min"
    Min' -> fn "min"
    Max -> fn "max"
    Max' -> fn "max"
    Step -> fn "step"
    Step' -> fn "step"
    Pow -> fn "pow"
    Distance -> fn "distance"
    Dot -> fn "dot"
    Cross -> fn "cross"
    Reflect -> fn "reflect"
    Atan2 -> fn "atan2"
    MkVec2 -> \x y -> vecPrefix (primof :: PrimType a) <> fn "vec2" x y
    where 
        fn s = \a b -> s <> "(" <> a <> "," <> b <> ")"
        binop s = \a b -> "(" <> a <> s <> b <> ")"

formatOp3 :: Op3 a b c d -> String -> String -> String -> String
formatOp3 (op :: Op3 a b c d) = case op of
    Clamp -> fn "clamp"
    Clamp' -> fn "clamp"
    Mix -> fn "mix"
    Mix' -> fn "mix"
    MixB -> fn "mixB"
    Smoothstep -> fn "smoothstep"
    Smoothstep' -> fn "smoothstep"
    Fma -> fn "fma"
    Faceforward -> fn "faceforward"
    Refract -> fn "refract"
    MkVec3 -> \x y z -> vecPrefix (primof :: PrimType a) <> fn "vec3" x y z
    where
        fn s = \a b c -> s <> "(" <> a <> "," <> b <> "," <> c <> ")"

formatOp4 :: Op4 a b c d e -> String -> String -> String -> String -> String
formatOp4 (op :: Op4 a b c d e) = case op of
    MkVec4 -> \x y z w -> vecPrefix (primof :: PrimType a) <> fn "vec4" x y z w
    where
        fn s = \a b c d -> s <> "(" <> a <> "," <> b <> "," <> c <> "," <> d <> ")"