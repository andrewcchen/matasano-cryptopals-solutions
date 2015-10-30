{-# OPTIONS_GHC -fno-warn-tabs #-}

import AES
import Common
import Xor

import Data.Function
import Data.List
import qualified Data.Vector.Generic as V
import System.Random

generate :: ByteVector -> ByteVector -> ByteVector -> ByteVector
generate key nonce input = encryptCTR key nonce $ prep V.++ sanitized V.++ post
	where
	prep = strToVec "comment1=cooking%20MCs;userdata="
	post = strToVec ";comment2=%20like%20a%20pound%20of%20bacon"
	sanitized = V.filter (/= 59) $ V.filter (/= 61) input

check :: ByteVector -> ByteVector -> ByteVector -> Bool
check key nonce input = ";admin=true;" `isInfixOf` vecToStr decrypted
	where decrypted = decryptCTR key nonce input

main = do
	putStrLn "=== Challange26 ==="
	key <- fmap V.fromList $ getStdRandom $ randomBytes 16
	nonce <- fmap V.fromList $ getStdRandom $ randomBytes 8
	let out = generate key nonce V.empty
	    diff = (xorBytes `on` strToVec) ";comment2=%2" ";admin=true;"
	    toXor = V.replicate 32 0 V.++ diff V.++ V.replicate 30 0
	    modified = xorBytes out toXor
	if check key nonce modified then putStrLn "Success!" else putStrLn "Failed!"
