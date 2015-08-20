import AES
import Base64
import Common
import Padding

import Data.Char
import Data.List
import qualified Data.Vector.Generic as V
import Data.Word
import System.Random

data Mode = ECB | CBC deriving (Eq, Show, Enum)

encryptionOracle :: ByteVector -> ByteVector -> ByteVector
encryptionOracle key input = encryptECB key $ pkcs7Pad 16 $ input V.++ secret
    where
    secret = base64Decode "\
    \Um9sbGluJyBpbiBteSA1LjAKV2l0aCBteSByYWctdG9wIGRvd24gc28gbXkg\
    \aGFpciBjYW4gYmxvdwpUaGUgZ2lybGllcyBvbiBzdGFuZGJ5IHdhdmluZyBq\
    \dXN0IHRvIHNheSBoaQpEaWQgeW91IHN0b3A/IE5vLCBJIGp1c3QgZHJvdmUg\
    \YnkK"

detectMode :: (ByteVector -> ByteVector) -> Mode
detectMode blackbox = if topFreq >= 14 then ECB else CBC
    where
    outputBlocks = chunksOf 16 $ blackbox $ V.replicate (16 * 16) 0
    topFreq = last $ sort $ map length $ group $ sort outputBlocks

detectBlockSize :: (ByteVector -> ByteVector) -> Int
detectBlockSize blackbox = go 4
    where
    guessCorrect guess = uncurry (==) outputs
        where
        genInputs x = map (V.replicate $ guess + x) [0..255]
        inputs = mapTuple genInputs (0, 1)
        outputs = mapTuple (map $ V.take guess . blackbox) inputs
    go guess = if guessCorrect guess then guess else go $ guess + 1

detectContentLength :: (ByteVector -> ByteVector) -> Int
detectContentLength blackbox = upperBound - (go 0)
    where
    upperBound = V.length $ blackbox V.empty
    go guess = if len > upperBound then guess else go $ guess + 1
        where len = V.length $ blackbox $ V.replicate guess 0

append :: [a] -> a -> [a]
append l e = l ++ [e]

decryptContent :: (ByteVector -> ByteVector) -> [Word8]
decryptContent blackbox = go 0 []
    where
    contentLength = detectContentLength blackbox
    go offset acc = if offset < contentLength then go (offset + 1) newAcc
                                              else acc
        where
        padCount = 15 - (offset `rem` 16)
        padding = replicate padCount 0
        blockNum = offset `quot` 16
        blockStart = blockNum * 16
        blockInit = take 15 $ drop blockStart $ padding ++ acc
        posBlocks = map (V.fromList . append blockInit) [0..255]
        posEncBlocks = map (V.take 16 . blackbox) posBlocks
        encBlocks = chunksOf 16 $ blackbox $ V.fromList padding
        actEncBlock = map V.fromList encBlocks !! blockNum
        Just byte = actEncBlock `elemIndex` posEncBlocks
        newAcc = acc `append` fromIntegral byte

main = do
    putStrLn "=== Challange12 ==="
    key <- fmap V.fromList $ getStdRandom $ randomBytes 16
    let blackbox = encryptionOracle key
    putStr "Mode: "
    putStrLn $ show $ detectMode blackbox
    putStr "Block size: "
    putStrLn $ show $ detectBlockSize blackbox
    putStrLn "Message: "
    putStrLn $ map (chr . fromIntegral) $ decryptContent blackbox
