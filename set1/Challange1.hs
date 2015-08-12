import Base64
import Common
import Hex

main = do
    putStrLn "=== Challange1 ==="
    putStrLn $ base64Encode $ hexDecode
           "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d"
