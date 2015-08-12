module Word24
( Word24
, toWord24
) where

import Common

import Data.Bits
import Data.Word

import GHC.Enum

newtype Word24 = Word24 { toWord32 :: Word32 } deriving (Eq, Ord)

toWord24 :: Integral a => a -> Word24
toWord24 i = Word24 $ fromIntegral i .&. 0xffffff

instance Bounded Word24 where
    minBound = 0
    maxBound = 0xffffff

instance Enum Word24 where
    succ x
        | x /= maxBound = x + 1
        | otherwise     = succError "Word24"
    pred x
        | x /= minBound = x - 1
        | otherwise     = predError "Word24"
    toEnum i
        | i >= 0 && i <= fromIntegral (maxBound::Word24)
                        = toWord24 i
        | otherwise     = toEnumError "Word24" i (minBound::Word24, maxBound::Word24)
    fromEnum            = fromEnum . toWord32
    enumFrom            = boundedEnumFrom
    enumFromThen        = boundedEnumFromThen

instance Integral Word24 where
    quot      x y = toWord24 $ quot (toWord32 x) (toWord32 y)
    rem       x y = toWord24 $ rem  (toWord32 x) (toWord32 y)
    div       x y = toWord24 $ div  (toWord32 x) (toWord32 y)
    mod       x y = toWord24 $ mod  (toWord32 x) (toWord32 y)
    quotRem   x y = (quot x y, rem x y)
    divMod    x y = (div  x y, mod x y)
    toInteger x   = toInteger $ toWord32 x

instance Num Word24 where
    (+)         x y = toWord24 $ (+)    (toWord32 x) (toWord32 y)
    (-)         x y = toWord24 $ (-)    (toWord32 x) (toWord32 y)
    (*)         x y = toWord24 $ (*)    (toWord32 x) (toWord32 y)
    negate      x   = toWord24 $ negate (toWord32 x)
    abs         x   = toWord24 $ abs    (toWord32 x)
    signum      x   = toWord24 $ signum (toWord32 x)
    fromInteger i   = toWord24 i

instance Read Word24 where
    readsPrec p s = map (mapFst toWord24) $ readsPrec p s

instance Real Word24 where
    toRational x = toRational (toWord32 x)

instance Show Word24 where
    showsPrec p x = showsPrec p $ toWord24 x

instance FiniteBits Word24 where
    finiteBitSize _ = 24
    countLeadingZeros  x = countLeadingZeros (toWord32 x) - 8
    countTrailingZeros x = countTrailingZeros $ toWord32 x

instance Bits Word24 where
    {-# INLINE shift #-}
    {-# INLINE bit #-}
    {-# INLINE testBit #-}

    (.&.)      x y = toWord24 $ (.&.)      (toWord32 x) (toWord32 y)
    (.|.)      x y = toWord24 $ (.|.)      (toWord32 x) (toWord32 y)
    xor        x y = toWord24 $ xor        (toWord32 x) (toWord32 y)
    complement x   = toWord24 $ complement (toWord32 x)
    shiftL     x i = toWord24 $ shiftL     (toWord32 x) i
    shiftR     x i = toWord24 $ shiftR     (toWord32 x) i
    rotate     x i = toWord24 $ rotate     (toWord32 x) i

    bitSizeMaybe i = Just (finiteBitSize i)
    bitSize      i = finiteBitSize i
    isSigned     _ = False
    popCount       = popCount . toWord32
    bit            = bitDefault
    testBit        = testBitDefault
