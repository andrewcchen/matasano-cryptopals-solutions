{-# OPTIONS_GHC -fno-warn-tabs #-}

import AES
import Base64
import Common

import System.IO

main = do
	putStrLn "=== Challange7 ==="
	handle <- openFile "7.txt" ReadMode
	enc <- fmap (base64Decode . filter (/= '\n')) $ hGetContents handle
	putStr $ vecToStr $ decryptECB (strToVec "YELLOW SUBMARINE") enc
