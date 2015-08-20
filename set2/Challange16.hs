import AES
import Common
import Padding
import Xor

import Data.Function
import Data.List
import qualified Data.Vector.Generic as V
import Control.Monad.Trans.State
import System.Random

generate :: ByteVector -> ByteVector -> ByteVector -> ByteVector
generate key iv input = encryptCBC key iv toEncrypt
    where
    prep = strToVec "comment1=cooking%20MCs;userdata="
    post = strToVec ";comment2=%20like%20a%20pound%20of%20bacon"
    sanitized = V.filter (/= 59) $ V.filter (/= 61) input
    toEncrypt = pkcs7Pad 16 $ prep V.++ sanitized V.++ post

check :: ByteVector -> ByteVector -> ByteVector -> Bool
check key iv input = ";admin=true;" `isInfixOf` vecToStr decrypted
    where
    Just decrypted = pkcs7Unpad $ decryptCBC key iv input

main = do
    putStrLn "=== Challange16 ==="
    key <- fmap V.fromList $ getStdRandom $ randomBytes 16
    iv <- fmap V.fromList $ getStdRandom $ randomBytes 16
    let out = generate key iv $ V.replicate 16 0
        diff = (xorBytes `on` strToVec) ";comment2=%20lik" ";admin=true;com="
        toXor = V.replicate 32 0 V.++ diff V.++ V.replicate 32 0
        modified = xorBytes out toXor
    if check key iv modified then putStrLn "Sucess!" else putStrLn "Failure!"
