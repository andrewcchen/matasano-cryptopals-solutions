import AES
import Common
import Padding

import Data.List
import qualified Data.Vector.Generic as V
import Control.Monad
import Control.Monad.Trans.State.Lazy
import System.Random

data Mode = ECB | CBC deriving (Eq, Show, Enum)

encryptionOracle :: Mode -> ByteVector -> StdGen -> (ByteVector, StdGen)
encryptionOracle mode input rng = flip runState rng $ do
    let randomByteVector      = liftM V.fromList . state . randomBytes
        randomCountByteVector = state (randomR (5, 10)) >>= randomByteVector
    before <- randomCountByteVector
    after  <- randomCountByteVector
    key    <- randomByteVector 16
    iv     <- randomByteVector 16
    let content = before V.++ input V.++ after
        encrypt = case mode of ECB -> encryptECB key
                               CBC -> encryptCBC key iv
    return $ encrypt $ pkcs7Pad 16 content

detectMode :: (ByteVector -> ByteVector) -> Mode
detectMode blackbox = if topFreq >= 14 then ECB else CBC
    where
    outputBlocks = chunksOf 16 $ blackbox $ V.replicate (16 * 16) 0
    topFreq = last $ sort $ map length $ group $ sort outputBlocks

testDetectMode :: IO Bool
testDetectMode = do
    mode <- liftM toEnum $ getStdRandom (randomR (0, 1)) :: IO Mode
    rng  <- newStdGen
    let blackbox input = fst $ encryptionOracle mode input rng
    return $ mode == detectMode blackbox

main = do
    putStrLn "=== Challange11 ==="
    result <- liftM and $ sequence $ replicate 100 testDetectMode
    if result then putStrLn "100 Tests Passed"
              else putStrLn "Tests Failed"
