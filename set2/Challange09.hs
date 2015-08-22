import Common
import Padding

import Data.Maybe

import Test.QuickCheck

main = do
    putStrLn "=== Challange9 ==="
    quickCheck $ \bs v -> bs > 0 ==> fromJust (pkcs7Unpad (pkcs7Pad bs v)) === v
