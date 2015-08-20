module Padding
( pkcs7Pad
, pkcs7Unpad
) where

import Common
import qualified Data.Vector.Generic as V

pkcs7Pad :: Int -> ByteVector -> ByteVector
pkcs7Pad blockSize bytes = bytes V.++ (V.replicate padding $ fromIntegral padding)
    where
    padding = blockSize - V.length bytes `rem` blockSize

takeFromTail :: Int -> ByteVector -> ByteVector
takeFromTail len bytes = V.drop (V.length bytes - len) bytes

dropFromTail :: Int -> ByteVector -> ByteVector
dropFromTail len bytes = V.take (V.length bytes - len) bytes

pkcs7Unpad :: ByteVector -> Maybe ByteVector
pkcs7Unpad bytes = if paddingCorrect then Just $ dropFromTail padCount bytes
                                     else Nothing
    where
    paddingCorrect = padCount > 0 && expPadding == actPadding
    expPadding = V.replicate padCount $ fromIntegral padCount
    actPadding = takeFromTail padCount bytes
    padCount = fromIntegral $ V.last bytes
