{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import Amount
import Control.Monad (when)
import Control.Monad.IO.Class
import Data.IORef
import Data.Ratio
import GHC.TypeLits
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Staking
import System.Environment
import Text.Show.Pretty

data Details = Details
  { timeSinceGenesis :: Double,
    initialStakeICP :: ICP,
    finalStakeICP :: ICP,
    dissolveDelayYears :: Double,
    dissolveStartYears :: Double,
    stakingDurationYears :: Double,
    votingPowerPercValue :: Amount 6,
    mintingPercValue :: Amount 6,
    earningsICP :: ICP,
    avgApy :: Double
  }
  deriving (Show)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [stake, delay, dissolve, duration] ->
      print $
        computeStake
          469_000_000
          (const 0.67)
          (const 0.15)
          (5 * oneMonthSeconds)
          (read stake)
          (read delay * oneYearSeconds)
          (read dissolve * oneYearSeconds)
          (read duration * oneYearSeconds)
    [ floor
        . (* fromIntegral oneYearSeconds)
        . (read :: String -> Amount 6) ->
        start,
      (read :: String -> Amount 6) -> voting,
      (read :: String -> Amount 6) -> minting,
      (read :: String -> ICP) -> stake,
      floor
        . (* fromIntegral oneYearSeconds)
        . (read :: String -> Amount 6) ->
        delay,
      floor
        . (* fromIntegral oneYearSeconds)
        . (read :: String -> Amount 6) ->
        dissolve,
      floor
        . (* fromIntegral oneYearSeconds)
        . (read :: String -> Amount 6) ->
        duration
      ] -> do
        let final =
              computeStake
                469_000_000
                (const voting)
                (const minting)
                start
                stake
                delay
                dissolve
                duration
            apy :: Double
            apy
              | duration > 0 =
                100
                  * ( (fromRational (toRational (final / stake)))
                        ** ( fromIntegral oneYearSeconds
                               / fromIntegral duration
                           )
                          - 1
                    )
              | otherwise = 0
        pPrint
          Details
            { timeSinceGenesis =
                fromIntegral start
                  / fromIntegral oneYearSeconds,
              initialStakeICP = stake,
              finalStakeICP = final,
              dissolveDelayYears =
                fromIntegral delay
                  / fromIntegral oneYearSeconds,
              dissolveStartYears =
                fromIntegral dissolve
                  / fromIntegral oneYearSeconds,
              stakingDurationYears =
                fromIntegral duration
                  / fromIntegral oneYearSeconds,
              votingPowerPercValue = voting,
              mintingPercValue = minting,
              earningsICP = final - stake,
              avgApy = apy
            }
    _ -> do
      apys <- newIORef []
      _ <-
        checkSequential $
          Group
            "icp-forecast"
            [ ("prop_perc", prop_perc),
              ("prop_limit", withTests 30 (prop_limit apys))
            ]
      pPrint =<< readIORef apys

genAmountFrac ::
  (MonadGen m, KnownNat n) =>
  Range Integer ->
  Range Integer ->
  m (Amount n)
genAmountFrac num den =
  Amount <$> ((%) <$> Gen.integral num <*> Gen.integral den)

genAmount ::
  (MonadGen m, KnownNat n) =>
  Range Integer ->
  m (Amount n)
genAmount = flip genAmountFrac (Range.singleton 1)

prop_perc :: Property
prop_perc = property $ do
  n <- forAll $ Gen.integral (Range.linear 0 1_000_000_000)
  diff (percentageOfSupply n) (>=) 0.05
  diff (percentageOfSupply n) (<=) 0.10

prop_limit :: IORef [Details] -> Property
prop_limit apys = property $ do
  initialSupply <-
    forAll $ genAmount (Range.linear 469_000_000 1_000_000_000)
  let votingPowerPerc = Amount (34 % 100)
  -- votingPowerPerc <-
  --   forAll $ genAmountFrac (Range.linear 1 100) (Range.singleton 100)
  let mintingPerc = Amount (5 % 100)
  -- mintingPerc <-
  --   forAll $ genAmountFrac (Range.linear 1 100) (Range.singleton 100)
  startTime <- forAll $ Gen.integral (Range.linear 0 1_000_000_000)
  stake <- forAll $ genAmount (Range.linear 1 1_000_000)
  let delay = 8 * oneYearSeconds
  -- delay <- forAll $ Gen.integral (Range.linear 0 (8 * oneYearSeconds))
  dissolve <- forAll $ Gen.integral (Range.linear 0 (20 * oneYearSeconds))
  duration <- forAll $ Gen.integral (Range.linear 0 (20 * oneYearSeconds))
  let final =
        computeStake
          initialSupply
          (const votingPowerPerc)
          (const mintingPerc)
          startTime
          stake
          delay
          dissolve
          duration
      apy :: Double
      apy
        | duration > 0 =
          100
            * ( (fromRational (toRational (final / stake)))
                  ** (fromIntegral oneYearSeconds / fromIntegral duration) - 1
              )
        | otherwise = 0
  -- traceM $ "initialSupply = " ++ show (initialSupply)
  -- traceM $ "votingPowerPerc = " ++ show (votingPowerPerc)
  -- traceM $ "mintingPerc = " ++ show (mintingPerc)
  -- traceM $ "startTime = " ++ show (startTime)
  -- traceM $ "stake = " ++ show (stake)
  -- traceM $ "delay = " ++ show (delay)
  -- traceM $ "duration = " ++ show (duration)
  -- traceM $ "final = " ++ show final
  when (apy > 0) $
    liftIO $
      atomicModifyIORef
        apys
        ( \xs ->
            ( Details
                { timeSinceGenesis =
                    fromIntegral startTime / fromIntegral oneYearSeconds,
                  initialStakeICP = stake,
                  finalStakeICP = final,
                  dissolveDelayYears =
                    fromIntegral delay / fromIntegral oneYearSeconds,
                  dissolveStartYears =
                    fromIntegral dissolve / fromIntegral oneYearSeconds,
                  stakingDurationYears =
                    fromIntegral duration / fromIntegral oneYearSeconds,
                  votingPowerPercValue = votingPowerPerc,
                  mintingPercValue = mintingPerc,
                  earningsICP = final - stake,
                  avgApy = apy
                } :
              xs,
              ()
            )
        )
  diff stake (<=) final
