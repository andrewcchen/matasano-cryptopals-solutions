{-# OPTIONS_GHC -fno-warn-tabs #-}

import AES
import Common
import Xor

import qualified Data.Vector.Generic as V
import System.Random

recoverKey :: ByteVector -> (ByteVector -> Maybe ByteVector) -> Maybe ByteVector
recoverKey enc decCheck = do
	let c1 = V.take 16 enc
	p' <- decCheck $ c1 V.++ V.replicate 16 0 V.++ c1
	let p'1 = V.take 16 p'
	    p'3 = V.take 16 $ V.drop 32 p'
	return $ xorBytes p'1 p'3

main = do
	putStrLn "=== Challange27 ==="
	key <- fmap V.fromList $ getStdRandom $ randomBytes 16
	let check enc = let dec = decryptCBC key key enc
	                in if V.any (> 127) dec then Just dec else Nothing
	    enc = encryptCBC key key $ strToVec message
	    Just recovered = recoverKey enc check
	if recovered == key then putStrLn "Success!" else putStrLn "Failure!"
	where
	message = "comment1=cooking%20MCs;comment2=%20like%20a%20pound%20of%20bacon"
