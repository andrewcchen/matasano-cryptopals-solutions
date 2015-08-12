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
    where shortLen = V.length b

xorWithSameLength :: ByteVector -> ByteVector -> ByteVector
xorWithSameLength = V.zipWith xor

xorBytes :: ByteVector -> ByteVector -> ByteVector
xorBytes a b
    | V.length a < V.length b      = xorBytes b a
    | (V.length b) == 1            = xorWithSingleByte a $ V.head b
    | (V.length a) == (V.length b) = xorWithSameLength a b
    | otherwise                    = xorWithRepeatingBytes a b

breakSingleKeyXor :: ByteVector -> Word8
breakSingleKeyXor bytes = fst $ head $ sortOn snd $ zip keyRange scores
    where keyRange = [32..126] -- The printable characters
          scores = map scoreCharFreq $ map (xorWithSingleByte bytes) keyRange

guessKeySize :: ByteVector -> [Int]
guessKeySize bytes = map fst $ sortOn snd $ zip keySizeRange dists
    where keySizeRange = [2 .. min 40 $ V.length bytes]
          dists = map (flip blockHammingDistance bytes) keySizeRange

breakRepeatingKeyXor :: Int -> ByteVector -> ByteVector
breakRepeatingKeyXor keySize bytes = V.fromList $ keys
    where blocks = map V.fromList $ transpose $ chunksOf keySize bytes
          keys = map breakSingleKeyXor blocks
