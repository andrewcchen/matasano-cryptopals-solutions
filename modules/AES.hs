module AES
( encryptECB
, decryptECB
, encryptCBC
, decryptCBC
) where

import Common
import Xor

import qualified Crypto.Cipher.AES as A
import qualified Data.Vector.Generic as V
import Data.Vector.Storable.ByteString

encryptECB :: ByteVector -> ByteVector -> ByteVector
encryptECB key bytes
    | V.length key /= 16 = error "key must be 128 bits"
    | V.length bytes `rem` 16 /= 0 = error "bytes must be a multiple of 128 bits"
    | otherwise = byteStringToVector $ A.encryptECB context bytesUnpacked
        where context = A.initAES $ vectorToByteString key
              bytesUnpacked = vectorToByteString bytes

decryptECB :: ByteVector -> ByteVector -> ByteVector
decryptECB key bytes
    | V.length key /= 16 = error "key must be 128 bits"
    | V.length bytes `rem` 16 /= 0 = error "bytes must be a multiple of 128 bits"
    | otherwise = byteStringToVector $ A.decryptECB context bytesUnpacked
        where context = A.initAES $ vectorToByteString key
              bytesUnpacked = vectorToByteString bytes

encryptCBC :: ByteVector -> ByteVector -> ByteVector -> ByteVector
encryptCBC key iv bytes
    | V.length key /= 16 = error "key must be 128 bits"
    | V.length iv /= 16 = error "iv must be 128 bits"
    | V.length bytes `rem` 16 /= 0 = error "bytes must be a multiple of 128 bits"
    | otherwise = go iv bytes
        where context = A.initAES $ vectorToByteString key
              go prev bs
                  | V.length bs == 0 = V.empty
                  | otherwise = enc V.++ go enc (V.drop 16 bs)
                      where enc = byteStringToVector $ A.encryptECB context curr
                            curr = vectorToByteString $ xorBytes (V.take 16 bs) prev

decryptCBC :: ByteVector -> ByteVector -> ByteVector -> ByteVector
decryptCBC key iv bytes
    | V.length key /= 16 = error "key must be 128 bits"
    | V.length iv /= 16 = error "iv must be 128 bits"
    | V.length bytes `rem` 16 /= 0 = error "bytes must be a multiple of 128 bits"
    | otherwise = xorBytes dec prevs
        where dec = decryptECB key bytes
              prevs = V.take (V.length bytes) $ iv V.++ bytes
