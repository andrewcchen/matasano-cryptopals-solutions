import AES
import Base64
import Common

import qualified Data.Vector.Generic as V
import System.IO

main = do
    putStrLn "=== Challange10 ==="
    enc <- fmap (base64Decode . filter (/= '\n')) $ hGetContents =<< openFile "10.txt" ReadMode
    putStr $ vecToStr $ decryptCBC (strToVec "YELLOW SUBMARINE") (V.replicate 16 0) enc
