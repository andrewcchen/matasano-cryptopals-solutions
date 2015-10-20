{-# OPTIONS_GHC -fno-warn-tabs #-}

import Common
import Hex
import Xor

main = do
	putStrLn "=== Challange5 ==="
	putStrLn $ hexEncode $ xorBytes message key
	where
	key = strToVec "ICE"
	message = strToVec "Burning 'em, if you ain't quick and nimble\nI go crazy when I hear a cymbal"
