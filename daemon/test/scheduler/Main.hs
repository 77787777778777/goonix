{-# LANGUAGE GHC2024 #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Control.Monad (forM_, unless)
import Data.ByteString (ByteString)
import qualified Data.Map.Strict as Map
import Goonix.Build.Scheduler

assert :: String -> Bool -> IO ()
assert name condition = unless condition (error ("scheduler property failed: " ++ name))

chain :: Int -> Bool -> State
chain slots keepGoing = foldl add (initialState slots keepGoing) [1 .. 5 :: Int]
  where
    add state n = either error id (addJob (label n) (if n == 1 then [] else [label (n - 1)]) state)
    label n = fromIntegral n `seq` (if n == 1 then "one" else if n == 2 then "two" else if n == 3 then "three" else if n == 4 then "four" else "five")

running :: State -> Int
running state = Map.size (Map.filter ((== Running) . status) (jobs state))

main :: IO ()
main = do
  let initial = chain 1 True
      (one, actions1) = step Tick initial
  assert "one initial start" (actions1 == [Start "one"])
  assert "slot limit" (running one <= 1)
  let (two, actions2) = step (Completed "one") one
  assert "dependency released" (actions2 == [Start "two"])
  let (failed, actions3) = step (FailedEvent "two") two
      blocked = failed
  assert "failure blocks dependent" (status (jobs blocked Map.! "three") == Blocked)
  assert "failure is reported" (Fail "three" `elem` actions3)
  let independent = either error id (addJob "independent" [] (initialState 1 True))
      (withoutSlots, noStart) = step Tick independent
  assert "job runs" (noStart == [Start "independent"])
  let (failedIndependent, _) = step (FailedEvent "independent") withoutSlots
      (stopped, _) = step Tick (failedIndependent { keepGoing = False })
  assert "failure reaches terminal state" (status (jobs stopped Map.! "independent") == Failed)
  forM_ [0 .. 3] $ \limit -> do
    let state = initialState limit True
    assert "nonnegative slots" (running state <= limit)
  putStrLn "scheduler properties: PASS"
