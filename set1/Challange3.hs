import Common
import Hex
import Xor

import Data.Char
import qualified Data.Vector.Generic as V

main = do
    putStrLn "=== Challange3 ==="
    putStrLn $ vecToStr $ xorWithSingleByte enc $ breakSingleKeyXor enc
    where enc = hexDecode "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736"
