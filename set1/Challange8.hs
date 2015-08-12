import AES
import Common
import Hex

import Data.List
import System.IO

main = do
    putStrLn "=== Challange8 ==="
    encs <- fmap (map hexDecode . lines) $ hGetContents =<< openFile "8.txt" ReadMode
    putStr "ECB encrypted string detected: #"
    putStrLn $ show $ fst $ last $ sortOn snd $ zip [1..] $ map scoreRep encs
    where scoreRep = sum . map (^2) . map length . group . sort . chunksOf 16
