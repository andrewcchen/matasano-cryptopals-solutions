{-# OPTIONS_GHC -fno-warn-tabs #-}

module Hamming
( hammingDistance
, blockHammingDistance
) where

import Common

import Data.Bits
import Data.Function
import Data.List
import qualified Data.Vector.Generic as V
import Data.Word

hammingDistance :: ByteVector -> ByteVector -> Int
hammingDistance a b = V.sum $ V.map popCount $ V.zipWith xor a b

chunksOfEven :: Int -> ByteVector -> [ByteVector]
chunksOfEven size = go
	where
	go bytes = case V.splitAt size bytes of
	                (a,b) | V.null a  -> []
	                      | V.null b  -> []
	                      | otherwise -> a : go b

intDiv :: (Integral a, Integral b, Fractional c) => a -> b -> c
intDiv x y = fromIntegral x / fromIntegral y

mean :: (Integral a, Foldable t, Fractional b) => t a -> b
mean l = sum l `intDiv` length l

blockHammingDistance :: Int -> ByteVector -> Double
blockHammingDistance blockSize bytes = mean dists / fromIntegral blockSize
	where
	dists = zipWith hammingDistance chunks $ tail chunks
	chunks = chunksOfEven blockSize bytes
