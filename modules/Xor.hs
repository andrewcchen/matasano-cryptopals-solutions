module Xor
( xorWithSingleByte
, xorBytes
, breakSingleKeyXor
, guessKeySize
, breakRepeatingKeyXor
) where

import CharFreq
import Common
import Hamming

import Data.Bits
import Data.Char
import Data.List
import Data.Word
import qualified Data.Vector.Generic as V

xorWithSingleByte :: ByteVector -> Word8 -> ByteVector
xorWithSingleByte bytes byte = V.map (xor byte) bytes

xorWithRepeatingBytes :: ByteVector -> ByteVector -> ByteVector
xorWithRepeatingBytes a b = V.imap (\i x -> xor x $ b V.! (i `mod` shortLen)) a
    where
    shortLen = V.length b

xorWithSameLength :: ByteVector -> ByteVector -> ByteVector
xorWithSameLength = V.zipWith xor

xorBytes :: ByteVector -> ByteVector -> ByteVector
xorBytes a b
    | V.length a < V.length b      = xorBytes b a
    | (V.length b) == 1            = xorWithSingleByte a $ V.head b
    | (V.length a) == (V.length b) = xorWithSameLength a b
    | otherwise                    = xorWithRepeatingBytes a b

breakSingleKeyXor :: ByteVector -> Word8
breakSingleKeyXor bytes = head $ sortOn scoreKey keyRange
    where
    keyRange = [0..255]
    scoreKey = scoreByCharFreq . xorWithSingleByte bytes

guessKeySize :: ByteVector -> [Int]
guessKeySize bytes = sortOn (flip blockHammingDistance bytes) keySizeRange
    where
    keySizeRange = [2 .. min 40 $ V.length bytes]

breakRepeatingKeyXor :: Int -> ByteVector -> ByteVector
breakRepeatingKeyXor keySize bytes = V.fromList $ keys
    where
    keys = map breakSingleKeyXor blocks
    blocks = map V.fromList $ transpose $ chunksOf keySize bytes
