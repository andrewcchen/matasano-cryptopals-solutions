import AES
import Common
import Padding

import Data.List
import Data.List.Split
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Vector.Generic as V
import Control.Monad
import System.Random

parse :: String -> M.Map String String
parse str = M.fromList $ map listToTuple $ map (splitOn "=") $ splitOn "&" str
    where listToTuple (x1:x2:[]) = (x1, x2)

profileFor :: String -> String
profileFor email = "email=" ++ strip email ++ "&uid=10&role=user"
    where strip = filter (/= '&') . filter (/= '=')

work :: (String -> ByteVector) -> ByteVector
work blackbox = V.slice 0 32 encProfile V.++ encAdminBlock
    where padString = vecToStr . pkcs7Pad 16 . strToVec
          adminText = replicate 10 ' ' ++ padString "admin"
          encAdminBlock = V.slice 16 16 $ blackbox adminText
          encProfile = blackbox "foo@gmail.com"

main = do
    putStrLn "=== Challange13 ==="
    key <- liftM V.fromList $ getStdRandom $ randomBytes 16
    let blackbox = encryptECB key . pkcs7Pad 16 . strToVec . profileFor
        decrypt = parse . vecToStr . fromJust . pkcs7Unpad . decryptECB key
        sucess = decrypt (work blackbox) M.! "role" == "admin"
    if sucess then putStrLn "Sucess!" else putStrLn "Failure!"
