import Base64
import Common
import Hex
import Xor

import System.IO

main = do
    putStrLn "=== Challange6 ==="
    handle <- openFile "6.txt" ReadMode
    enc <- fmap (base64Decode . filter (/= '\n')) $ hGetContents handle
    putStrLn $ vecToStr $ breakXor enc
    where
    breakXor enc = xorBytes enc $ breakRepeatingKeyXor (guessKeySize) enc
