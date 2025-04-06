-- SchedulerState.hs
-- 
-- This module defines the 'SchedulerState' data type, which represents the current state of the scheduling system.
-- The state contains the information about the current employee schedule, the number of hours assigned to each employee
-- on a weekly basis, and the daily shifts assigned to each employee.
-- 
-- The 'SchedulerState' data type is used throughout the scheduling system to track the progress of the schedule and
-- help manage and modify the assignments.
-- 
module SchedulerState (
    SchedulerState(..)
) where

import ShiftSchedule
import Employee
import EmployerRequirements
import qualified Data.Map as Map

-- Scheduler State Type
data SchedulerState = SchedulerState {
    schedule :: ShiftSchedule,                      -- The schedule (assignments) for employees
    weeklyAssignedHours :: Map.Map String Int,      -- Map of employee names to their weekly assigned hours
    dailyShifts :: Map.Map String [String]          -- Map of employee names to their daily shifts
} deriving (Show, Eq)
