{-# OPTIONS_GHC -fno-warn-tabs #-}

import Common
import Hex
import Xor

import Data.Function

main = do
	putStrLn "=== Challange2 ==="
	putStrLn $ hexEncode $ (xorBytes `on` hexDecode)
		"1c0111001f010100061a024b53535009181c" "686974207468652062756c6c277320657965"
