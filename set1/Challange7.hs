import AES
import Base64
import Common

import System.IO

main = do
    putStrLn "=== Challange7 ==="
    enc <- fmap (base64Decode . filter (/= '\n')) $ hGetContents =<< openFile "7.txt" ReadMode
    putStr $ vecToStr $ decryptECB (strToVec "YELLOW SUBMARINE") enc
