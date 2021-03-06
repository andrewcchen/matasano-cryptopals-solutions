{-# OPTIONS_GHC -fno-warn-tabs #-}

import AES
import Base64
import Common
import Padding

import Data.Char
import Data.Function
import Data.List
import qualified Data.Vector.Generic as V
import Data.Word
import Control.Monad.Trans.State.Lazy
import System.Random

encryptionOracle :: ByteVector -> ByteVector -> StdGen -> (ByteVector, StdGen)
encryptionOracle key input rng = flip runState rng $ do
	let secret = base64Decode "\
	      \Um9sbGluJyBpbiBteSA1LjAKV2l0aCBteSByYWctdG9wIGRvd24gc28gbXkg\
	      \aGFpciBjYW4gYmxvdwpUaGUgZ2lybGllcyBvbiBzdGFuZGJ5IHdhdmluZyBq\
	      \dXN0IHRvIHNheSBoaQpEaWQgeW91IHN0b3A/IE5vLCBJIGp1c3QgZHJvdmUg\
	      \YnkK"
	    byteCountRange = (5, 10)
	prefixByteCount <- state $ randomR byteCountRange
	prefixBytes <- fmap V.fromList $ state $ randomBytes prefixByteCount
	let toEncrypt = prefixBytes V.++ input V.++ secret
	return $ encryptECB key $ pkcs7Pad 16 $ toEncrypt

append :: [a] -> a -> [a]
append l e = l ++ [e]

decryptContent :: (ByteVector -> StdGen -> (ByteVector, StdGen))
               -> StdGen
               -> ([Word8], StdGen)
decryptContent blackbox rng = flip runState rng $ do
	let sortByFreq = map head . sortOn length . group . sort
	    mostFreqBlk = last . sortByFreq . chunksOf 16
	magicStrRep <- fmap (V.replicate $ 42 * 16) $ state random
	magicStrRepEnc <- fmap mostFreqBlk $ state $ blackbox magicStrRep

	(magicStr, magicStrEnc) <- fix $ \go -> do
		pad <- fmap (flip V.replicate 0) $ state $ randomR (0, 15)
		str <- fmap V.fromList $ state $ randomBytes 16
		enc <- state $ blackbox $ pad V.++ str V.++ V.take 16 magicStrRep
		if magicStrRepEnc `elem` chunksOf 16 enc
			then let blks = chunksOf 16 enc
			         Just magicRepEncInd = magicStrRepEnc `elemIndex` blks
			         strEncInd = magicRepEncInd - 1
			     in return (str, blks !! strEncInd)
			else go -- try again


	let contentLength = 138
	    fixW2 x1 x2 f = fix f x1 x2
	fixW2 0 [] $ \go offset acc -> if offset >= contentLength
	                                   then return acc
	                                   else do
		let encrypt content = do
			pad <- fmap (flip V.replicate 0) $ state $ randomR (0, 15)
			enc <- state $ blackbox $ pad V.++ magicStr V.++ content
			let blks = chunksOf 16 enc
			case magicStrEnc `elemIndex` blks of
			     Nothing -> encrypt content -- try again
			     Just magicStrEncInd -> return $ drop (magicStrEncInd + 1) blks

		let padCount = 15 - (offset `rem` 16)
		    padding = replicate padCount 0
		    blockNum = offset `quot` 16
		    blockStart = blockNum * 16
		    blockInit = take 15 $ drop blockStart $ padding ++ acc

		let posBlocks = map (V.fromList . append blockInit) [0..255]
		posEncBlocks <- mapM (fmap head . encrypt) posBlocks
		actEncBlock <- fmap (!! blockNum) (encrypt $ V.fromList padding)
		let Just byte = actEncBlock `elemIndex` posEncBlocks
		    newAcc = acc `append` fromIntegral byte
		go (offset + 1) newAcc

main = do
	putStrLn "=== Challange14 ==="
	key <- fmap V.fromList $ getStdRandom $ randomBytes 16
	let blackbox = encryptionOracle key
	putStrLn "Message: "
	decrypted <- getStdRandom $ decryptContent blackbox
	putStrLn $ map (chr . fromIntegral) decrypted
