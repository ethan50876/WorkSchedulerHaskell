-- SchedulerContext.hs
module SchedulerContext (
    SchedulerContext(..)
) where

import Employee (Employee(..))
import EmployerRequirements (EmployerRequirements(..))

-- Scheduler Context Type
data SchedulerContext = SchedulerContext {
    days :: [String],
    hours :: [Int],
    shiftLengths :: (Int, Int),
    employees :: [Employee],
    reqs :: EmployerRequirements
} deriving (Show, Eq)
