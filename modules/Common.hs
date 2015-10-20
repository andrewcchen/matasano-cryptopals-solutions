{-# OPTIONS_GHC -fno-warn-tabs #-}

module Common
( Vector
, ByteVector
, mapFst
, mapSnd
, mapTuple
, vecToStr
, strToVec
, chunksOf
, randomBytes
, appendBits
, splitBitsAt
) where

import Data.Bits
import Data.Char
import Data.Word
import qualified Data.Vector.Generic as V
import qualified Data.Vector.Storable as VS
import Control.Arrow
import Control.Monad
import System.Random
import Test.QuickCheck

type Vector = VS.Vector

type ByteVector = Vector Word8

instance (Arbitrary a, VS.Storable a) => Arbitrary (VS.Vector a) where
	arbitrary = fmap VS.fromList arbitrary
	shrink = map VS.fromList . shrinkList shrink . VS.toList

mapFst :: (a -> b) -> (a, c) -> (b, c)
mapFst f (x,y) = (f x,y)

mapSnd :: (a -> b) -> (c, a) -> (c, b)
mapSnd f (x,y) = (x,f y)

mapTuple :: (a -> b) -> (a, a) -> (b, b)
mapTuple = join (***)

vecToStr :: ByteVector -> String
vecToStr = V.toList . V.map (chr . fromIntegral)

strToVec :: String -> ByteVector
strToVec = V.map (fromIntegral . ord) . V.fromList

chunksOf :: Int -> ByteVector -> [[Word8]]
chunksOf size = go
	where
	go bytes = case V.splitAt size bytes of
	                (a,b) | V.null a  -> []
	                      | otherwise -> V.toList a : go b

randomBytes :: Int -> StdGen -> ([Word8], StdGen)
randomBytes count rng = go count rng []
	where
	go c g acc
		| c == 0 = (acc, g)
		| otherwise = let (byte, newG) = random g
		              in go (c - 1) newG (byte:acc)

appendBits :: (Bits a, Integral a, FiniteBits b, Integral b) => a -> b -> a
appendBits x bits = x `shiftL` finiteBitSize bits .|. fromIntegral bits

splitBitsAt :: (Bits a, Integral a, Bits b, Integral b) => [Int] -> a -> [b]
splitBitsAt boundaries bits = map (fromIntegral . (bits `shiftR`)) boundaries
