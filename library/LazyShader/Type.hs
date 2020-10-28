{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module LazyShader.Type where

import Linear (V2, V3, V4)


class Typed a where
    typeof :: Type a


data Type a where
    Prim :: Primitive a => Type a
    Vec2 :: Primitive a => PrimType a -> Type (V2 a)
    Vec3 :: Primitive a => PrimType a -> Type (V3 a)
    Vec4 :: Primitive a => PrimType a -> Type (V4 a)    


class (Primitive t, Typed a) => GenType t a where
    pureG :: GenType t a => t -> a
    mapG :: GenType t a => (t -> t) -> a -> a
    -- mapG' :: (GenType t a, GenType t' b) => (t -> t') -> a -> b
    -- foldG :: (t -> t -> t) -> t -> a -> t


data PrimType a where
    Bool :: PrimType Bool
    Int :: PrimType Int
    Word :: PrimType Word
    Float :: PrimType Float


class Typed a => Primitive a where
    primof :: PrimType a


instance Primitive Bool where
    primof = Bool

instance Primitive Int where
    primof = Int

instance Primitive Word where
    primof = Word

instance Primitive Float where
    primof = Float


instance Typed Bool where
    typeof = Prim

instance Typed Int where
    typeof = Prim

instance Typed Word where
    typeof = Prim

instance Typed Float where
    typeof = Prim

instance Primitive a => Typed (V2 a) where
    typeof = Vec2 primof

instance Primitive a => Typed (V3 a) where
    typeof = Vec3 primof

instance Primitive a => Typed (V4 a) where
    typeof = Vec4 primof


instance (Primitive a, Typed a) => GenType a a where
    pureG x = x
    mapG f x = f x

instance (Primitive a, Typed a) => GenType a (V2 a) where
    pureG = pure
    mapG = fmap

instance (Primitive a, Typed a) => GenType a (V3 a) where
    pureG = pure
    mapG = fmap

instance (Primitive a, Typed a) => GenType a (V4 a) where
    pureG = pure
    mapG = fmap
