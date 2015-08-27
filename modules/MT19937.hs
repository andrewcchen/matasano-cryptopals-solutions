module MT19937
( MT19937
, twist
, temper
, fromState
, seed
, next
) where

import Common

import Control.Exception
import Control.Monad
import Data.Bits
import qualified Data.Vector.Generic as V
import qualified Data.Vector.Generic.Mutable as VM
import Data.Word

data MT19937 = MT19937 !Int (Vector Word32)

instance Show MT19937 where
    show _ = "MT19937"

(w, n, m, r) = (32, 624, 397, 31) :: (Int, Int, Int, Int)
a = 0x9908B0DF                    :: Word32
(u, d) = (11, 0xFFFFFFFF)         :: (Int, Word32)
(s, b) = (7,  0x9D2C5680)         :: (Int, Word32)
(t, c) = (15, 0xEFC60000)         :: (Int, Word32)
l = 18                            :: Int
f = 1812433253                    :: Word32

checkStateLength :: Vector Word32 -> a -> a
checkStateLength state
    | V.length state /= n = error "state must be n words long"
    | otherwise = id

twist :: Vector Word32 -> Vector Word32
twist state = checkStateLength state $ V.create $ do
    newState <- VM.new n
    forM_ [0..n - 1] $ \k -> do
        let lowerMask = (1 `shiftL` r) - 1
            upperMask = complement lowerMask
            getX i = if i < n then return $ state V.! i
                            else VM.read newState $ i `rem` n
        xku <- fmap (.&. upperMask) $ getX k
        xkp1l <- fmap (.&. lowerMask) $ getX (k + 1)
        let x = xku .|. xkp1l
            xa = (x `shiftR` 1) `xor` if x .&. 1 == 0 then 0 else a
        xkpm <- getX (k + m)
        VM.write newState k $ xkpm `xor` xa
    return newState

temper :: Word32 -> Word32
temper x = z
    where
    y1 = x  `xor` ((x  `shiftR` u) .&. d)
    y2 = y1 `xor` ((y1 `shiftL` s) .&. b)
    y3 = y2 `xor` ((y2 `shiftL` t) .&. c)
    z =  y3 `xor`  (y3 `shiftR` l)

fromState :: Vector Word32 -> MT19937
fromState state = checkStateLength state $ MT19937 0 state

seed :: Word32 -> MT19937
seed x0 = fromState $ twist state
    where
    state = V.create $ do
        newState <- VM.new n
        VM.write newState 0 x0
        forM_ [1..n - 1] $ \i -> do
            xim1 <- VM.read newState $ i - 1
            let xi = f * (xim1 `xor` (xim1 `shiftR` (w - 2))) + fromIntegral i
            VM.write newState i xi
        return newState

next :: MT19937 -> (Word32, MT19937)
next (MT19937 pos state) = assert (pos < n) (output, MT19937 newPos newState)
    where
    output = temper $ state V.! pos
    newPos = (pos + 1) `rem` n
    newState = if pos == n - 1 then twist state else state
