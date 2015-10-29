{-# OPTIONS_GHC -fno-warn-tabs #-}

import AES
import Base64
import Common
import Xor

import qualified Data.Vector.Generic as V
import Data.Word
import System.IO
import System.Random

import Control.Exception
import Debug.Trace

recover :: ByteVector -> (ByteVector -> Int -> ByteVector -> ByteVector)
        -> ByteVector
recover enc edit = xorBytes enc $ edit enc 0 $ V.replicate len 0
	where len = V.length enc

main = do
	putStrLn "=== Challange25 ==="
	handle <- openFile "25.txt" ReadMode
	oldenc <- fmap (base64Decode . filter (/= '\n')) $ hGetContents handle
	let text = decryptECB (strToVec "YELLOW SUBMARINE") oldenc
	key <- fmap V.fromList $ getStdRandom $ randomBytes 16
	nonce <- fmap V.fromList $ getStdRandom $ randomBytes 8
	let enc = encryptCTR key nonce text
	putStrLn $ vecToStr $ recover enc (edit key nonce)
	where
	edit key nonce ciphertext off newtext = pre V.++ new V.++ post
		where
		new = xorBytes newtext $ genCTRKeyStream (off, off + len - 1) key nonce
		pre = V.slice 0 off ciphertext
		post = V.drop (off + len) ciphertext
		len = V.length newtext
