{-# OPTIONS_GHC -fno-warn-tabs #-}

module AES
( encryptECB
, decryptECB
, encryptCBC
, decryptCBC
, genCTRKeyStream
, encryptCTR
, decryptCTR
) where

import Common
import Xor

import qualified Crypto.Cipher.AES as A
import Data.Bits
import qualified Data.Vector.Generic as V
import Data.Vector.Storable.ByteString
import Data.Word

import Control.Exception.Base

encryptECB :: ByteVector -> ByteVector -> ByteVector
encryptECB key bytes
	| V.length key /= 16 = error "key must be 128 bits"
	| V.length bytes `rem` 16 /= 0 = error "bytes must be a multiple of 128 bits"
	| otherwise = byteStringToVector $ A.encryptECB keyProcessed bytesUnpacked
		where
		keyProcessed = A.initAES $ vectorToByteString key
		bytesUnpacked = vectorToByteString bytes

decryptECB :: ByteVector -> ByteVector -> ByteVector
decryptECB key bytes
	| V.length key /= 16 = error "key must be 128 bits"
	| V.length bytes `rem` 16 /= 0 = error "bytes must be a multiple of 128 bits"
	| otherwise = byteStringToVector $ A.decryptECB keyProcessed bytesUnpacked
		where
		keyProcessed = A.initAES $ vectorToByteString key
		bytesUnpacked = vectorToByteString bytes

encryptCBC :: ByteVector -> ByteVector -> ByteVector -> ByteVector
encryptCBC key iv bytes
	| V.length key /= 16 = error "key must be 128 bits"
	| V.length iv /= 16 = error "iv must be 128 bits"
	| V.length bytes `rem` 16 /= 0 = error "bytes must be a multiple of 128 bits"
	| otherwise = go iv bytes
		where
		go prev bs
			| V.length bs == 0 = V.empty
			| otherwise = enc V.++ go enc (V.drop 16 bs)
				where
				enc = byteStringToVector $ A.encryptECB keyProcessed curr
				curr = vectorToByteString $ xorBytes (V.take 16 bs) prev
		keyProcessed = A.initAES $ vectorToByteString key

decryptCBC :: ByteVector -> ByteVector -> ByteVector -> ByteVector
decryptCBC key iv bytes
	| V.length key /= 16 = error "key must be 128 bits"
	| V.length iv /= 16 = error "iv must be 128 bits"
	| V.length bytes `rem` 16 /= 0 = error "bytes must be a multiple of 128 bits"
	| otherwise = xorBytes dec prevs
		where
		dec = decryptECB key bytes
		prevs = V.take (V.length bytes) $ iv V.++ bytes

genCTRKeyStream :: (Int, Int) -> ByteVector -> ByteVector -> ByteVector
genCTRKeyStream (start, end) key nonce
	| V.length key /= 16 = error "key must be 128 bits"
	| V.length nonce /= 8 = error "nonce must be 64 bits"
	| otherwise = chop $ V.concat $ map keyStreamBlock [startBlock..endBlock]
	where
	startBlock = start `quot` 16
	endBlock = end `quot` 16
	startDrop = start - startBlock * 16
	endDrop = (endBlock * 16 + 15) - end
	chop = dropFromTail endDrop . V.drop startDrop
		where
		dropFromTail len bytes = V.take (V.length bytes - len) bytes
	keyStreamBlock ind = encryptECB key ctr
		where
		ctr = nonce V.++ word64ToBytes (fromIntegral ind)
			where
			word64ToBytes :: Word64 -> ByteVector
			word64ToBytes = V.fromList . splitBitsAt [0, 8..63]

encryptCTR :: ByteVector -> ByteVector -> ByteVector -> ByteVector
encryptCTR key nonce bytes = xorBytes bytes keyStreamBytes
	where
	keyStreamBytes = genCTRKeyStream (0, V.length bytes - 1) key nonce

decryptCTR :: ByteVector -> ByteVector -> ByteVector -> ByteVector
decryptCTR = encryptCTR
