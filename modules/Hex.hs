module Hex
( hexDecode
, hexEncode
) where

import Common

import Data.Bits
import Data.Char
import qualified Data.Vector.Generic as V
import Data.Word

hexDecode :: String -> ByteVector
hexDecode str = V.fromList $ combineNibblePairs $ map digitToInt str
    where combineNibbles hi lo = (hi `shiftL` 4) .|. lo
          combineNibblePairs [] = []
          combineNibblePairs (_:[]) = error "odd number of characters in hex string"
          combineNibblePairs (hi:lo:rest) = (fromIntegral $ combineNibbles hi lo) : (combineNibblePairs rest)

hexEncode :: ByteVector -> String
hexEncode bytes = map (intToDigit . fromIntegral) $ foldMap splitNibbles $ V.toList bytes
    where splitNibbles i = [(i `shiftR` 4) .&. 0xf, i .&. 0xf]
