module EmployerRequirements (EmployerRequirements(..)) where


import Data.Map as Map

data EmployerRequirements = EmployerRequirements
  { workHours        :: (Int, Int)
  , shiftLengths     :: (Int, Int)
  , criticalMinimums :: Map String Int
  } deriving (Show, Eq)
