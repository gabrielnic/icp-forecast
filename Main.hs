{-# LANGUAGE NumericUnderscores #-}

module Main where

import Staking
import System.Environment

main :: IO ()
main = do
  [stake, delay, duration] <- getArgs
  print $
    computeStake
      469_000_000
      0.67
      0.15
      (5 * oneMonthSeconds)
      (read stake)
      (read delay * oneYearSeconds)
      (read duration * oneYearSeconds)
