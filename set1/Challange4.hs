import CharFreq
import Common
import Hex
import Xor

import Data.List
import System.IO

main = do
    putStrLn "=== Challange4 ==="
    encs <- fmap (map hexDecode . lines) $ hGetContents =<< openFile "4.txt" ReadMode
    putStrLn $ vecToStr $ head $ sortOn scoreCharFreq $ map breakXor encs
    where breakXor enc = xorWithSingleByte enc $ breakSingleKeyXor enc
