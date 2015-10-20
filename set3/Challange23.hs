{-# OPTIONS_GHC -fno-warn-tabs #-}

import Common
import MT19937

import Control.Monad
import Control.Monad.Trans.State.Strict
import qualified Data.Vector.Generic as V
import qualified Data.Vector.Storable as VS
import Data.Word

import Test.QuickCheck
import Test.QuickCheck.Arbitrary

newtype S = S { getState :: VS.Vector Word32 }

instance Arbitrary S where
	arbitrary = fmap (S . V.fromList) $ sequence $ replicate 624 arbitrary

instance Show S where
	showsPrec d = showsPrec d . getState

clone :: State MT19937 MT19937
clone = fmap (fromState . twist . V.fromList) clonedState
	where
	clonedState = sequence $ replicate 624 $ fmap untemper $ state next

main = do
	putStrLn "=== Challange23 ==="
	quickCheck $ \x -> untemper (temper x) === x
	quickCheck $ \s ->
		let (oldG, newG) = runState clone $ seed s
		in fst (next oldG) === fst (next newG)
	quickCheck $ \s ->
		let (oldG, newG) = runState clone $ fromState (getState s)
		in fst (next oldG) === fst (next newG)
