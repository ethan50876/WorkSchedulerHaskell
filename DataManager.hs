module DataManager where

import Data.Set as Set
import Data.Map as Map

data Employee = Employee
  { name        :: String
  , availability :: (Int, Int)
  , minHours    :: Int
  , maxHours    :: Int
  , roles       :: Set String
  , daysOff     :: Set String
  } deriving (Show, Eq)

data EmployerRequirements = EmployerRequirements
  { workHours        :: (Int, Int)
  , shiftLengths     :: (Int, Int)
  , criticalMinimums :: Map String Int
  } deriving (Show, Eq)