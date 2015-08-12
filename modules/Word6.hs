module Word6
( Word6
, toWord6
) where

import Common

import Data.Bits
import Data.Word

import GHC.Enum

newtype Word6 = Word6 { toWord8 :: Word8 } deriving (Eq, Ord)

toWord6 :: Integral a => a -> Word6
toWord6 i = Word6 $ fromIntegral i .&. 0x3f

instance Bounded Word6 where
    minBound = 0
    maxBound = 0x3f

instance Enum Word6 where
    succ x
        | x /= maxBound = x + 1
        | otherwise     = succError "Word6"
    pred x
        | x /= minBound = x - 1
        | otherwise     = predError "Word6"
    toEnum i
        | i >= 0 && i <= fromIntegral (maxBound::Word6)
                        = toWord6 i
        | otherwise     = toEnumError "Word6" i (minBound::Word6, maxBound::Word6)
    fromEnum            = fromEnum . toWord8
    enumFrom            = boundedEnumFrom
    enumFromThen        = boundedEnumFromThen

instance Integral Word6 where
    quot      x y = toWord6 $ quot (toWord8 x) (toWord8 y)
    rem       x y = toWord6 $ rem  (toWord8 x) (toWord8 y)
    div       x y = toWord6 $ div  (toWord8 x) (toWord8 y)
    mod       x y = toWord6 $ mod  (toWord8 x) (toWord8 y)
    quotRem   x y = (quot x y, rem x y)
    divMod    x y = (div  x y, mod x y)
    toInteger x   = toInteger $ toWord8 x

instance Num Word6 where
    (+)         x y = toWord6 $ (+)    (toWord8 x) (toWord8 y)
    (-)         x y = toWord6 $ (-)    (toWord8 x) (toWord8 y)
    (*)         x y = toWord6 $ (*)    (toWord8 x) (toWord8 y)
    negate      x   = toWord6 $ negate (toWord8 x)
    abs         x   = toWord6 $ abs    (toWord8 x)
    signum      x   = toWord6 $ signum (toWord8 x)
    fromInteger i   = toWord6 $ i

instance Read Word6 where
    readsPrec p s = map (mapFst toWord6) $ readsPrec p s

instance Real Word6 where
    toRational x = toRational (toWord8 x)

instance Show Word6 where
    showsPrec p x = showsPrec p $ toWord8 x

instance FiniteBits Word6 where
    finiteBitSize _ = 6
    countLeadingZeros  x = countLeadingZeros (toWord8 x) - 2
    countTrailingZeros x = countTrailingZeros $ toWord8 x

instance Bits Word6 where
    {-# INLINE shift #-}
    {-# INLINE bit #-}
    {-# INLINE testBit #-}

    (.&.)      x y = toWord6 $ (.&.)      (toWord8 x) (toWord8 y)
    (.|.)      x y = toWord6 $ (.|.)      (toWord8 x) (toWord8 y)
    xor        x y = toWord6 $ xor        (toWord8 x) (toWord8 y)
    complement x   = toWord6 $ complement (toWord8 x)
    shiftL     x i = toWord6 $ shiftL     (toWord8 x) i
    shiftR     x i = toWord6 $ shiftR     (toWord8 x) i
    rotate     x i = toWord6 $ rotate     (toWord8 x) i

    bitSizeMaybe i = Just (finiteBitSize i)
    bitSize      i = finiteBitSize i
    isSigned     _ = False
    popCount       = popCount . toWord8
    bit            = bitDefault
    testBit        = testBitDefault
