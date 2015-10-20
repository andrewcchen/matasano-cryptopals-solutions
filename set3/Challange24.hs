{-# OPTIONS_GHC -fno-warn-tabs #-}

import Common
import MT19937
import Xor

import Data.List
import Data.Maybe
import qualified Data.Vector.Generic as V
import Data.Word
import Control.Monad
import Control.Monad.Trans.State
import System.Random hiding (next)

genKeyStream :: Int -> Word32 -> ByteVector
genKeyStream len s = V.fromList $ fst $ go len $ seed s
	where
	go :: Int -> MT19937 -> ([Word8], MT19937)
	go len gen
		| len <= 0 = ([], gen)
		| otherwise = flip runState gen $ fmap (take $ min len 4) $
			liftM2 (++) (fmap split $ state next) (state $ (go $ len - 4))
			where
			split = splitBitsAt [24, 16, 8, 0] :: Word32 -> [Word8]

encryptMT19937 :: Word32 -> ByteVector -> ByteVector
encryptMT19937 s bytes = xorBytes bytes $ genKeyStream (V.length bytes) s

recoverKey :: (ByteVector -> IO ByteVector) -> IO Word32
recoverKey enc = do
	let text = "AAAAAAAAAAAAAAAA"
	out <- enc $ strToVec text
	return $ fromJust $ flip find [0..65535] $ \k ->
		text `isInfixOf` vecToStr (xorBytes out (genKeyStream (V.length out) k))

main = do
	putStrLn "=== Challange24 ==="
	key <- fmap fromIntegral (getStdRandom random :: IO Word16)
	let enc bytes = do
		prep <- getStdRandom . randomBytes =<< getStdRandom (randomR (5, 10))
		return $ encryptMT19937 key (V.fromList prep V.++ bytes)
	recovered <- recoverKey enc
	if recovered == key then putStrLn "Success!" else putStrLn "Failure!"
