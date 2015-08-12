import AES
import Base64
import CharFreq
import Common
import Hex
import Xor

import Test.QuickCheck

main = do
    putStrLn "===== Set1 ====="
    quickCheck $ \v -> base64Decode (base64Encode v) == v
    quickCheck $ \v -> hexDecode (hexEncode v) == v
