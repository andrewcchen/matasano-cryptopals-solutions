import AES
import Base64
import Common

import Data.Vector.Generic as V

main = do
    putStrLn "=== Challange18 ==="
    putStrLn $ vecToStr $ decryptCTR key nonce message
    where
    message = base64Decode "L77na/nrFsKvynd6HzOoG7GHTLXsTVu9qvY/2syLXzhPweyyMTJULu/6/kXX0KSvoOLSFQ=="
    key = strToVec "YELLOW SUBMARINE"
    nonce = V.replicate 8 0
