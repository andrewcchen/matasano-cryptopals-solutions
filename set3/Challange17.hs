import AES
import Common
import Padding
import Xor

import Control.Monad
import Data.Bits
import Data.Maybe
import qualified Data.Vector.Generic as V
import System.Random

decryptContent :: ByteVector -> ByteVector -> (ByteVector -> Bool) -> ByteVector
decryptContent enc iv chkPad = V.concat $ zipWith decryptBlock blks prevBlks
    where
    blks = map V.fromList $ chunksOf 16 enc
    prevBlks = iv : init blks
    decryptBlock currBlk prevBlk = tailBytes 16 `xorBytes` prevBlk
        where
        tailBytes len = thisByte `V.cons` prevBytes
            where
            thisByte = fromJust $ tryWithPad 0 `mplus` tryWithPad 255
            prevBytes = if len == 1 then V.empty else tailBytes $ len - 1
            tryWithPad padByte = case filter (chkPad . gen) [0..255] of
                                 (x:[]) -> Just $ x `xor` fromIntegral len
                                 xs -> Nothing
                where
                gen b = pad `V.snoc` b V.++ prevXored V.++ currBlk
                    where
                    pad = V.replicate (16 - len) padByte
                    prevXored = prevBytes `xorWithSingleByte` fromIntegral len

main = do
    putStrLn "=== Challange17 ==="
    let randomBlock = fmap V.fromList $ getStdRandom $ randomBytes 16
    key <- randomBlock
    iv  <- randomBlock
    let secret = "Decrypting... Success!"
    let enc = encryptCBC key iv $ pkcs7Pad 16 $ strToVec secret
    let chkPad = isJust . pkcs7Unpad . decryptCBC key iv
    putStrLn $ vecToStr $ fromJust $ pkcs7Unpad $ decryptContent enc iv chkPad
