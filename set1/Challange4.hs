import CharFreq
import Common
import Hex
import Xor

import Data.List
import System.IO

main = do
    putStrLn "=== Challange4 ==="
    handle <- openFile "4.txt" ReadMode
    encs <- fmap (map hexDecode . lines) $ hGetContents handle
    putStrLn $ vecToStr $ head $ sortOn scoreByCharFreq $ map breakXor encs
    where
    breakXor enc = xorWithSingleByte enc $ breakSingleKeyXor enc
