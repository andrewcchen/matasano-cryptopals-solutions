module Base64
( base64Decode
, base64Encode
) where

import Common
import Word6
import Word24

import Data.Bits
import qualified Data.Map as M
import qualified Data.Vector.Generic as V
import Data.Word

mapLookup :: Ord k => M.Map k v -> k -> v
mapLookup m k = (\(Just x) -> x) $ M.lookup k m

base64ToChar :: Word6 -> Char
base64ToChar = mapLookup bcMap
    where
    bcMap = M.fromList $ zip [0..63] $ ['A'..'Z']++['a'..'z']++['0'..'9']++['+','/']

charToBase64 :: Char -> Word6
charToBase64 = mapLookup cbMap
    where
    cbMap = M.fromList $ zip (['A'..'Z']++['a'..'z']++['0'..'9']++['+','/']) [0..63]

infixl 8 <<<<
(<<<<) :: (Bits a) => a -> Int -> a
(<<<<) = shiftL

infixl 8 >>>>
(>>>>) :: (Bits a) => a -> Int -> a
(>>>>) = shiftR

appendBits :: (Bits a, Integral a, FiniteBits b, Integral b) => a -> b -> a
appendBits x bits = (x <<<< (finiteBitSize bits)) .|. (fromIntegral bits)

splitBits :: (Bits a, Integral a, Bits b, Integral b) => a -> [Int] -> [b]
splitBits bits boundaries = map (fromIntegral . (bits >>>>)) boundaries

sixBitsToEightBits :: [Word6] -> [Word8]
sixBitsToEightBits ss
    | length ss /= 4 = error "Wrong number of six-bits"
    | otherwise = splitBits combined [16, 8, 0]
    where
    combined = foldl appendBits 0 ss :: Word24

eightBitsToSixBits :: [Word8] -> [Word6]
eightBitsToSixBits es
    | length es /= 3 = error "Wrong number of eight-bits"
    | otherwise = splitBits combined [18, 12, 6, 0]
    where
    combined = foldl appendBits 0 es :: Word24

base64Decode :: String -> ByteVector
base64Decode s = V.fromList $ go $ map charToBase64 $ filter (/= '=') s
    where
    go [] = []
    go (c1:[]) = take 1 $ go [c1, 0, 0, 0]
    go (c1:c2:[]) = take 1 $ go [c1, c2, 0, 0]
    go (c1:c2:c3:[]) = take 2 $ go [c1, c2, c3, 0]
    go cs = (sixBitsToEightBits $ take 4 cs) ++ (go $ drop 4 cs)

base64Encode :: ByteVector -> String
base64Encode bytes = base64Str ++ padding
    where
    base64Str = map base64ToChar $ go $ V.toList bytes
    padding = replicate padCount '='
    padCount = - (V.length bytes `mod` (-3))
    go [] = []
    go (b1:[]) = take 2 $ go [b1, 0, 0]
    go (b1:b2:[]) = take 3 $ go [b1, b2, 0]
    go bs = (eightBitsToSixBits $ take 3 bs) ++ (go $ drop 3 bs)
