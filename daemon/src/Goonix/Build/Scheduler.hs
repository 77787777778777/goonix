{-# LANGUAGE GHC2024 #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}
module Goonix.Build.Scheduler
  ( JobId
  , Event (..)
  , Action (..)
  , State (..)
  , Job (..)
  , Status (..)
  , initialState
  , addJob
  , step
  ) where

import Data.ByteString (ByteString)
import qualified Data.Map.Strict as Map

-- This model contains no IO.  The engine interprets Start/Finish/Failed
-- actions against the store; generated event orders test the same invariants.
type JobId = ByteString

data Status = Waiting | Running | Succeeded | Failed | Blocked
  deriving (Eq, Ord, Show)

data Job = Job
  { dependencies :: ![JobId]
  , status :: !Status
  } deriving (Eq, Show)

data State = State
  { jobs :: !(Map.Map JobId Job)
  , slots :: !Int
  , keepGoing :: !Bool
  } deriving (Eq, Show)

data Event = Tick | Completed JobId | FailedEvent JobId
  deriving (Eq, Show)

data Action = Start JobId | Finish JobId | Fail JobId | ReportFailure JobId
  deriving (Eq, Show)

initialState :: Int -> Bool -> State
initialState slots keepGoing = State Map.empty (max 0 slots) keepGoing

addJob :: JobId -> [JobId] -> State -> Either String State
addJob name dependencies state
  | Map.member name (jobs state) = Left "duplicate derivation job"
  | name `elem` dependencies = Left "self-dependent derivation job"
  | any (`Map.notMember` jobs state) dependencies = Left "unknown dependency"
  | otherwise = Right state { jobs = Map.insert name (Job dependencies Waiting) (jobs state) }

step :: Event -> State -> (State, [Action])
step event state = schedule $ case event of
  Completed name -> update name Succeeded state
  FailedEvent name -> update name Failed state
  Tick -> state
  where
    update name result current = case Map.lookup name (jobs current) of
      Just job | status job == Running -> current { jobs = Map.insert name job { status = result } (jobs current) }
      _ -> current

schedule :: State -> (State, [Action])
schedule state =
  let running = Map.size $ Map.filter ((== Running) . status) (jobs state)
      blocked = any ((== Failed) . status) (jobs state) && not (keepGoing state)
      candidates = [name | (name, job) <- Map.toList (jobs state), status job == Waiting,
        all (succeeded state) (dependencies job), not (blocked && hasFailedDependency state name)]
      (chosen, _rest) = splitAt (max 0 (slots state - running)) candidates
      state' = state { jobs = foldl (\js name -> Map.adjust (\job -> job { status = Running }) name js) (jobs state) chosen }
      failedDependents = [name | (name, job) <- Map.toList (jobs state'), status job == Waiting,
        any (failed state') (dependencies job)]
      state'' = state' { jobs = foldl (\js name -> Map.adjust (\job -> job { status = Blocked }) name js) (jobs state') failedDependents }
  in (state'', map Start chosen ++ map Fail failedDependents)
  where
    succeeded s name = maybe False ((== Succeeded) . status) (Map.lookup name (jobs s))
    failed s name = maybe False ((`elem` [Failed, Blocked]) . status) (Map.lookup name (jobs s))
    hasFailedDependency s name = maybe False (any (failed s) . dependencies) (Map.lookup name (jobs s))
