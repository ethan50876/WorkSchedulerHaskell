-- EmployerRequirements.hs
module EmployerRequirements (
    EmployerRequirements(..)
) where

import qualified Data.Map as Map

-- Employer Requirements Data Type
data EmployerRequirements = EmployerRequirements {
    workHours :: (Int, Int),
    shiftLengths :: (Int, Int),
    criticalMinimums :: [(String, Int)]
} deriving (Show, Eq)
