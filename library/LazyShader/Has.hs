{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
module LazyShader.Has where

import Linear (V2(..), V3(..), V4(..))


class HasX a b | a -> b where
    getX :: a -> b

class HasY a b | a -> b where
    getY :: a -> b
    
class HasZ a b | a -> b where
    getZ :: a -> b

class HasW a b | a -> b where
    getW :: a -> b


instance HasX (V2 a) a where
    getX (V2 x _) = x
    
instance HasY (V2 a) a where
    getY (V2 _ y) = y


instance HasX (V3 a) a where
    getX (V3 x _ _) = x
    
instance HasY (V3 a) a where
    getY (V3 _ y _) = y

instance HasZ (V3 a) a where
    getZ (V3 _ _ z) = z

    
instance HasX (V4 a) a where
    getX (V4 x _ _ _) = x
    
instance HasY (V4 a) a where
    getY (V4 _ y _ _) = y

instance HasZ (V4 a) a where
    getZ (V4 _ _ z _) = z

instance HasW (V4 a) a where
    getW (V4 _ _ _ w) = w
