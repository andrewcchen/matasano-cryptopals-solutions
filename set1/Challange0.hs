import Base64
import Hex

import Test.QuickCheck

main = do
    putStrLn "===== Set1 ====="
    quickCheck $ \v -> base64Decode (base64Encode v) == v
    quickCheck $ \v -> hexDecode (hexEncode v) == v
