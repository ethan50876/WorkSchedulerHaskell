-- SchedulerState.hs
module SchedulerState (
    SchedulerState(..)
) where

import ShiftSchedule (ShiftSchedule(..))
import Employee (Employee(..))
import EmployerRequirements (EmployerRequirements(..))
import qualified Data.Map as Map

-- Scheduler State Type
data SchedulerState = SchedulerState {
    schedule :: ShiftSchedule,
    weeklyAssignedHours :: Map.Map String Int,
    dailyShifts :: Map.Map String [String]
} deriving (Show, Eq)
