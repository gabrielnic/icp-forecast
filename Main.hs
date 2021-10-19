{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import Amount
import Control.Monad (when)
import Control.Monad.IO.Class
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.IORef
import Data.List (foldl', genericLength)
import Data.List.Split (chunksOf)
import Data.Ratio
import GHC.Generics
import GHC.TypeLits
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Staking
import System.Environment

data Stats = Stats
  { s_stake :: Double,
    s_earnings :: Double,
    s_apy :: Double
  }
  deriving (Show, Generic)

deriving instance ToJSON Stats

data Details = Details
  { timeSinceGenesis :: Double,
    initialStakeICP :: Double,
    finalStakeICP :: Double,
    dissolveDelayYears :: Double,
    dissolveStartYears :: Double,
    stakingDurationYears :: Double,
    votingPowerPercValue :: Amount 6,
    mintingPercValue :: Amount 6,
    yearlyStats :: [Stats],
    totalEarningsICP :: Double,
    compounding :: Bool,
    avgApy :: Double
  }
  deriving (Show, Generic)

deriving instance ToJSON Details

calcStats :: ICP -> [ICP] -> [Stats]
calcStats initialStake =
  reverse . snd
    . foldl'
      ( \(stake, acc) xs ->
          let earned = sum xs
           in ( stake + earned,
                Stats
                  { s_stake = fromRational (toRational stake),
                    s_earnings = fromRational (toRational earned),
                    s_apy =
                      apy
                        (oneDaySeconds * genericLength xs)
                        stake
                        (stake + earned)
                  } :
                acc
              )
      )
      (initialStake, [])
    . chunksOf 365

main :: IO ()
main = do
  args <- getArgs
  case args of
    [stake, delay, dissolve, duration, compound] ->
      print $
        read stake
          + sum
            ( computeStake
                469_000_000
                (const 0.67)
                (const 0.15)
                (5 * oneMonthSeconds)
                (read stake)
                (read delay * oneYearSeconds)
                (read dissolve * oneYearSeconds)
                (read duration * oneYearSeconds)
                (read compound)
            )
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
        duration,
      (read :: String -> Bool) -> compound
      ] -> do
        let earnings =
              computeStake
                469_000_000
                (const voting)
                (const minting)
                start
                stake
                delay
                dissolve
                duration
                compound
            final = stake + sum earnings
        BS.putStrLn $
          encode
            Details
              { timeSinceGenesis =
                  fromIntegral start
                    / fromIntegral oneYearSeconds,
                initialStakeICP = fromRational (toRational stake),
                finalStakeICP = fromRational (toRational final),
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
                yearlyStats = calcStats stake earnings,
                totalEarningsICP = fromRational (toRational (final - stake)),
                compounding = False,
                avgApy = apy duration stake final
              }
    _ -> do
      apys <- newIORef []
      _ <-
        checkSequential $
          Group
            "icp-forecast"
            [ ("prop_perc", prop_perc),
              ("prop_age_bonus", prop_age_bonus),
              ("prop_delay_bonus", prop_delay_bonus),
              ("prop_limit", prop_limit apys)
            ]
      BS.putStrLn . encode =<< readIORef apys

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

prop_age_bonus :: Property
prop_age_bonus = property $ do
  n <- forAll $ Gen.integral (Range.linear 0 4_000_000_000)
  diff (ageBonus n) (>=) 1
  diff (ageBonus n) (<=) 1.25

prop_delay_bonus :: Property
prop_delay_bonus = property $ do
  n <- forAll $ Gen.integral (Range.linear 0 4_000_000_000)
  diff (delayBonus n) (>=) 1
  diff (delayBonus n) (<=) 2

prop_limit :: IORef [Details] -> Property
prop_limit apys = property $ do
  initialSupply <-
    forAll $ genAmount (Range.linear 469_000_000 1_000_000_000)
  let votingPowerPerc = Amount (67 % 100)
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
  let earnings =
        computeStake
          initialSupply
          (const votingPowerPerc)
          (const mintingPerc)
          startTime
          stake
          delay
          dissolve
          duration
          False
      final = stake + sum earnings
      apy' = apy duration stake final
  -- traceM $ "initialSupply = " ++ show (initialSupply)
  -- traceM $ "votingPowerPerc = " ++ show (votingPowerPerc)
  -- traceM $ "mintingPerc = " ++ show (mintingPerc)
  -- traceM $ "startTime = " ++ show (startTime)
  -- traceM $ "stake = " ++ show (stake)
  -- traceM $ "delay = " ++ show (delay)
  -- traceM $ "duration = " ++ show (duration)
  -- traceM $ "final = " ++ show final
  when (apy' > 0) $
    liftIO $
      atomicModifyIORef
        apys
        ( \xs ->
            ( Details
                { timeSinceGenesis =
                    fromIntegral startTime / fromIntegral oneYearSeconds,
                  initialStakeICP = fromRational (toRational stake),
                  finalStakeICP = fromRational (toRational final),
                  dissolveDelayYears =
                    fromIntegral delay / fromIntegral oneYearSeconds,
                  dissolveStartYears =
                    fromIntegral dissolve / fromIntegral oneYearSeconds,
                  stakingDurationYears =
                    fromIntegral duration / fromIntegral oneYearSeconds,
                  votingPowerPercValue = votingPowerPerc,
                  mintingPercValue = mintingPerc,
                  yearlyStats = calcStats stake earnings,
                  totalEarningsICP = fromRational (toRational (final - stake)),
                  compounding = False,
                  avgApy = apy'
                } :
              xs,
              ()
            )
        )
  diff stake (<=) final

apy :: Seconds -> ICP -> ICP -> Double
apy duration stake final
  | duration > 0 =
    100
      * ( (fromRational (toRational (final / stake)))
            ** (fromIntegral oneYearSeconds / fromIntegral duration) - 1
        )
  | otherwise = 0
