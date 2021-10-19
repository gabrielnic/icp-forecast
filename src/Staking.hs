{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}

module Staking where

import Amount
import Control.Exception (assert)
import Control.Monad.State
import Data.Ratio

-- import Debug.Trace

type Seconds = Integer

type Percentage = Amount 6

type ICP = Amount 6

oneDaySeconds, oneYearSeconds, oneMonthSeconds :: Seconds
oneDaySeconds = 24 * 60 * 60
oneYearSeconds = (4 * 365 + 1) * oneDaySeconds `div` 4
oneMonthSeconds = oneYearSeconds `div` 12

maxDissolveDelay :: Seconds
maxDissolveDelay = 8 * oneYearSeconds

maxAgeBonus :: Seconds
maxAgeBonus = 4 * oneYearSeconds

ytd :: Seconds -> Seconds
ytd = (* oneYearSeconds)

percent :: Integer -> Percentage
percent n = Amount (n % 100)

-- From https://docs.google.com/document/d/1wP7zEcWdb2hE7L7of2Zhf6LxbWOqWnKGQ6LdIlDPzsk/edit
--
-- "Calling R0 the initial rate at genesis time G, Rf the final rate, and T
-- the time at which the rate becomes flat, the unique solution for G <= t <=
-- T is":
--         R(t) = Rf + (R0-Rf) [(t-T) / (G-T)]^2
percentageOfSupply :: Seconds -> Percentage
percentageOfSupply t = rf + (r0 - rf) * (v * v)
  where
    r0 = percent 10 -- initial rate at genesis time nG
    rf = percent 5 -- the final rate at time nT
    v = fromIntegral (nT - min t nT) / fromIntegral (nT - nG)
    nG = 0 -- genesis time (in seconds past genesis)
    nT = 8 * oneYearSeconds -- final time (in seconds past genesis)

data NNS = NNS
  { totalSupply :: ICP,
    votingPercentage :: Seconds -> Percentage,
    mintingPercentage :: Seconds -> Percentage,
    since :: Seconds
  }

votingPower :: ICP -> Seconds -> Seconds -> ICP
votingPower _ delay _ | delay < 6 * oneMonthSeconds = 0
votingPower stake delay age =
  let d = min delay maxDissolveDelay
      d_stake =
        stake + ((stake * fromIntegral d) / fromIntegral maxDissolveDelay)
      a = min age maxAgeBonus
   in d_stake + ((d_stake * fromIntegral a) / (4 * fromIntegral maxAgeBonus))

-- Compute rewards calculated on a given day
singleDay :: ICP -> Seconds -> Seconds -> State NNS ICP
singleDay stake delay age = assert (stake >= 0) $
  assert (delay >= 0) $
    assert (age >= 0) $ do
      nns <- get
      let vp = votingPower stake delay age
          dailyPercentage = percentageOfSupply (since nns) / 365.0
          dailyReward = totalSupply nns * dailyPercentage
          today = since nns + oneDaySeconds
          -- fraction = vp / (totalSupply nns * votingPercentage nns today)
          -- earned = dailyReward * fraction
          -- Another way of writing 'earned' is the following, which shows
          -- that if the voting population keeps pace with the increase in
          -- supply, one's daily take remains the same.
          earned
            | votingPercentage nns today > 0 =
              dailyPercentage * (vp / votingPercentage nns today)
            | otherwise = 0
          minted = max earned (dailyReward * mintingPercentage nns today)
      put
        nns
          { totalSupply = totalSupply nns + minted,
            since = today
          }
      -- traceM $ "vp = " ++ show vp
      -- traceM $ "dailyPercentage = " ++ show dailyPercentage
      -- traceM $ "dailyReward = " ++ show dailyReward
      -- traceM $ "today = " ++ show today
      -- traceM $ "earned = " ++ show earned
      -- traceM $ "minted = " ++ show minted
      assert (earned >= 0) $
        assert (dailyPercentage >= 0) $
          assert (vp == 0 || vp >= stake) $
            assert (votingPercentage nns today >= 0) $
              pure earned

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
  ICP
computeStake
  initialSupply
  votingPowerPerc
  mintingPerc
  startTime
  stake
  delay
  dissolve
  duration =
    assert (initialSupply > 0) $
      assert (startTime >= 0) $
        assert (stake >= 1) $
          assert (delay >= 0) $
            assert (duration >= 0) $
              evalState
                (go 0 stake)
                (NNS initialSupply votingPowerPerc mintingPerc startTime)
    where
      go :: Seconds -> ICP -> State NNS ICP
      go t s
        | t > duration = pure s
        | otherwise = do
          reward <-
            singleDay
              s
              ( if dissolve < 0 || t < dissolve
                  then delay
                  else max 0 (delay - (dissolve - t))
              )
              ( if dissolve < 0 || t < dissolve
                  then t
                  else 0
              )
          go (t + oneDaySeconds) (s + reward)

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
