import Base64
import Common
import Hex
import Xor

import System.IO

main = do
    putStrLn "=== Challange6 ==="
    enc <- fmap (base64Decode . filter (/= '\n')) $ hGetContents =<< openFile "6.txt" ReadMode
    putStrLn $ vecToStr $ xorBytes enc $ breakRepeatingKeyXor (head $ guessKeySize enc) enc
