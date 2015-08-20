import AES
import Base64
import Common

import qualified Data.Vector.Generic as V
import System.IO

main = do
    putStrLn "=== Challange10 ==="
    handle <- openFile "10.txt" ReadMode
    enc <- fmap (base64Decode . filter (/= '\n')) $ hGetContents handle
    putStr $ vecToStr $ decryptCBC key iv enc
    where
    key = strToVec "YELLOW SUBMARINE"
    iv = V.replicate 16 0
