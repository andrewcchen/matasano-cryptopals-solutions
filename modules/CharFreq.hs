{-# OPTIONS_GHC -fno-warn-tabs #-}

module CharFreq
( countCharFreq
, scoreByCharFreq
) where

import Common

import Data.Char
import qualified Data.Vector.Generic as V
import qualified Data.Vector.Generic.Mutable as VM
import Data.Word

charCode :: Num a => Char -> a
charCode = fromIntegral . ord

byteToLower :: Word8 -> Word8
byteToLower byte
	| isUpperCase = byte - charCode 'A' + charCode 'a'
	| otherwise = byte
	where
	isUpperCase = (charCode 'A') <= byte && byte <= (charCode 'Z')

countCharFreq :: ByteVector -> Vector Int
countCharFreq bytes = V.create $ do
	freqs <- VM.replicate 256 0
	V.forM_ bytes $ \byte -> do
		let ind = fromIntegral byte
		freq <- VM.read freqs ind
		VM.write freqs ind $ freq + 1
	return freqs

type CharCatagory = Vector Int

scoreByCharFreq :: ByteVector -> Double
scoreByCharFreq bytes = totalPenalty / totalChars
	where
	-- Constant Parameters --

	catPenaltyMethods = map (mapFst V.fromList) [
		(alphaCat,        (1 *)  . penalizeFreqAbsErr alphaExpCatProbs),
		(numCat,          (1 *)  . penalizeOccurWithThresh 0.1),
		(whitespaceCat,   (1 *)  . penalizeOccurWithThresh 0.2),
		(symbolCat,       (1 *)  . penalizeOccurWithThresh 0.1),
		(nonprintableCat, (10 *) . penalizeOccur)]

	alphaCat        = [97..122]
	numCat          = [48..57]
	whitespaceCat   = [32, 10] -- ' ', '\n'
	symbolCat       = [33..47]++[58..64]++[91..96]++[123..126]
	nonprintableCat = [0..9]++[11..31]++[127..255]

	alphaExpCatProbs = V.fromList [
		0.08167, 0.01492, 0.02782, 0.04253, 0.12702, 0.02228, 0.02015,
		0.06094, 0.06966, 0.00153, 0.00772, 0.04025, 0.02406, 0.06749,
		0.07507, 0.01929, 0.00095, 0.05987, 0.06327, 0.09056, 0.02758,
		0.00978, 0.02360, 0.00150, 0.01974, 0.00074]

	-- End Constant Parameters --

	charFreqs = countCharFreq $ V.map byteToLower bytes
	totalChars = fromIntegral $ V.length bytes
	totalPenalty = sum $ map (\(cat, p) -> p cat) catPenaltyMethods

	countCatChars :: Num a => CharCatagory -> a
	countCatChars cat = fromIntegral $ V.sum $ V.backpermute charFreqs cat

	penalizeOccurWithThresh :: Double -> CharCatagory -> Double
	penalizeOccurWithThresh thresh cat = countCatChars cat - allowedOccurs
		where
		allowedOccurs = thresh * totalChars

	penalizeOccur :: CharCatagory -> Double
	penalizeOccur = penalizeOccurWithThresh 0

	penalizeFreqAbsErr :: Vector Double -> CharCatagory -> Double
	penalizeFreqAbsErr expProbs cat = V.sum $ V.zipWith absDiff expectedFreqs actualFreqs
		where
		absDiff a b = abs $ a - b
		expectedFreqs = V.map (catCharCount *) expProbs
		actualFreqs = V.map fromIntegral $ V.backpermute charFreqs cat
		catCharCount = fromIntegral $ countCatChars cat
