{-# OPTIONS_GHC -fno-warn-tabs #-}

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
	where
	combineNibblePairs [] = []
	combineNibblePairs (_:[]) = error "odd number of characters in hex string"
	combineNibblePairs (hi:lo:rest) = (combineNibbles hi lo) : (combineNibblePairs rest)
	combineNibbles hi lo = fromIntegral $ (hi `shiftL` 4) .|. lo

hexEncode :: ByteVector -> String
hexEncode bytes = map toHexChar $ foldMap splitNibbles $ V.toList bytes
	where
	splitNibbles i = [(i `shiftR` 4) .&. 0xf, i .&. 0xf]
	toHexChar = intToDigit . fromIntegral
