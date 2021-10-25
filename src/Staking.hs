{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards #-}

module Staking where

import Amount
import Control.Exception (assert)
import Control.Monad.State hiding (lift)

type Seconds = Integer

type Percentage = Amount 6

type ICP = Amount 6

oneDaySeconds, oneYearSeconds, oneMonthSeconds :: Seconds
oneDaySeconds = 24 * 60 * 60
oneYearSeconds = (4 * 365 + 1) * oneDaySeconds `div` 4
oneMonthSeconds = oneYearSeconds `div` 12

sixMonthSeconds :: Seconds
sixMonthSeconds = 6 * oneMonthSeconds

maxDissolveDelay :: Seconds
maxDissolveDelay = 8 * oneYearSeconds

maxAgeBonus :: Seconds
maxAgeBonus = 4 * oneYearSeconds

-- From https://docs.google.com/document/d/1wP7zEcWdb2hE7L7of2Zhf6LxbWOqWnKGQ6LdIlDPzsk/edit
--
-- "Calling R0 the initial rate at genesis time G, Rf the final rate, and T
-- the time at which the rate becomes flat, the unique solution for G <= t <=
-- T is":
--         R(t) = Rf + (R0-Rf) [(t-T) / (G-T)]^2
percentageOfSupply :: Seconds -> Percentage
percentageOfSupply t = 0.05 * (v * v + 1)
  where
    v =
      fromIntegral (max 0 (maxDissolveDelay - t))
        / fromIntegral maxDissolveDelay

data NNS = NNS
  { totalSupply :: ICP,
    votingPercentage :: Seconds -> Percentage,
    mintingPercentage :: Seconds -> Percentage,
    since :: Seconds
  }

four_times_maxAgeBonus :: Seconds
four_times_maxAgeBonus = 4 * maxAgeBonus

ageBonus :: Seconds -> ICP
ageBonus age =
  fromIntegral (min age maxAgeBonus + four_times_maxAgeBonus)
    / fromIntegral four_times_maxAgeBonus

delayBonus :: Seconds -> ICP
delayBonus delay =
  fromIntegral (min delay maxDissolveDelay + maxDissolveDelay)
    / fromIntegral maxDissolveDelay

votingPower :: ICP -> Seconds -> Seconds -> ICP
votingPower stake delay age
  | delay < sixMonthSeconds = 0
  | otherwise = stake * delayBonus delay * ageBonus age

-- Compute rewards calculated on a given day
singleDay :: ICP -> Seconds -> Seconds -> State NNS ICP
singleDay stake delay age = state $ \nns@NNS {..} ->
  let vp = votingPower stake delay age
      dailyPercentage = percentageOfSupply since / 365.0
      dailyReward = totalSupply * dailyPercentage
      today = since + oneDaySeconds
      -- fraction = vp / (totalSupply nns * votingPercentage nns today)
      -- earned = dailyReward * fraction
      -- Another way of writing 'earned' is the following, which shows
      -- that if the voting population keeps pace with the increase in
      -- supply, one's daily take remains the same.
      vperc = votingPercentage today
      earned
        | vperc > 0 = dailyPercentage * (vp / vperc)
        | otherwise = 0
      minted = max earned (dailyReward * mintingPercentage today)
      supply = totalSupply + minted
   in ( earned,
        nns
          { totalSupply = supply,
            since = today
          }
      )

-- Given a set of starting conditions in terms of total supply of ICP, seconds
-- since genesis, an amount of ICP to be staked, a starting dissolve delay,
-- and a total time until disbursement, calculate what the final amount will
-- be assuming all other factors remain constant and all neuron holders merge
-- their maturity daily.
computeStake ::
  ICP ->
  (Seconds -> Percentage) ->
  (Seconds -> Percentage) ->
  Seconds ->
  ICP ->
  Seconds ->
  Seconds ->
  Seconds ->
  Bool ->
  [ICP]
computeStake
  initialSupply
  votingPowerPerc
  mintingPerc
  startTime
  stake
  delay
  dissolve
  duration
  compound =
    assert (initialSupply > 0) $
      assert (startTime >= 0) $
        assert (stake >= 1) $
          assert (delay >= 0) $
            assert (duration >= 0) $
              evalState
                (go 0 stake)
                (NNS initialSupply votingPowerPerc mintingPerc startTime)
    where
      go :: Seconds -> ICP -> State NNS [ICP]
      go t s
        | t > duration = pure []
        | otherwise = do
          reward <-
            singleDay
              s
              ( if dissolve < 0 || t < dissolve
                  then delay
                  else max 0 (delay - (t - dissolve))
              )
              ( if dissolve < 0 || t < dissolve
                  then t
                  else 0
              )
          (reward :)
            <$> go
              (t + oneDaySeconds)
              (if compound then s + reward else s)

-- Result: ~403184
scenario :: IO ()
scenario = do
  putStrLn "1,000 ICP"
  putStrLn $ "dissolve delay 8, dissolving in 8  = " ++ show (compute 8 0 8)
  putStrLn $ "dissolve delay 8, dissolving in 12 = " ++ show (compute 8 4 12)
  putStrLn $ "dissolve delay 4, dissolving in 8  = " ++ show (compute 4 4 8)
  putStrLn $ "dissolve delay 4, dissolving in 12 = " ++ show (compute 4 8 12)
  where
    compute delay dissolve duration =
      computeStake
        469_000_000
        (const 0.67)
        (const 0.15)
        (5 * oneMonthSeconds)
        1_000
        (delay * oneYearSeconds)
        (dissolve * oneYearSeconds)
        (duration * oneYearSeconds)
        False
