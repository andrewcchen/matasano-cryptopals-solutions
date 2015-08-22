module Padding
( pkcs7Pad
, pkcs7Unpad
) where

import Common

import qualified Data.Vector.Generic as V
import Data.Word

pkcs7Pad :: Word8 -> ByteVector -> ByteVector
pkcs7Pad blockSize bytes
    | blockSize == 0 = error "block size must be larger than 0"
    | otherwise = bytes V.++ V.replicate (fromIntegral padding) padding
    where
    padding = blockSize - fromIntegral (V.length bytes) `rem` blockSize

pkcs7Unpad :: ByteVector -> Maybe ByteVector
pkcs7Unpad bytes = if paddingCorrect then Just $ dropFromTail padCount bytes
                                     else Nothing
    where
    paddingCorrect = padCount > 0 && expPadding == actPadding
    expPadding = V.replicate padCount $ fromIntegral padCount
    actPadding = takeFromTail padCount bytes
    padCount = fromIntegral $ V.last bytes
    takeFromTail len bytes = V.drop (V.length bytes - len) bytes
    dropFromTail len bytes = V.take (V.length bytes - len) bytes
