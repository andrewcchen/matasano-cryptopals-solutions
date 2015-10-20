{-# OPTIONS_GHC -fno-warn-tabs #-}

import Common
import MT19937

import Control.Concurrent
import Control.Monad
import Data.Time.Clock.POSIX
import Data.Word
import System.Random hiding (next)

wait :: Int -> IO ()
wait n = do
	putStr "Waiting"
	replicateM_ n $ do
		threadDelay 1000000
		putStr "."
	putStr "\n"

crack :: Word32 -> IO Word32
crack out = do
	time <- fmap truncate getPOSIXTime
	return $ head $ filter ((out ==) . fst . next . seed) [time - 999..time]

main = do
	putStrLn "=== Challange22 ==="
	wait =<< getStdRandom (randomR (40, 400))
	s <- fmap truncate getPOSIXTime
	let rng = seed s
	wait =<< getStdRandom (randomR (40, 400))
	cs <- crack (fst $ next rng)
	if cs == s then putStrLn "Success!" else putStrLn "Failure!"
