-- SchedulerContext.hs
-- 
-- This module defines the 'SchedulerContext' data type, which represents the context for the scheduling system.
-- It stores essential information needed to schedule employees, including:
-- 
-- * Days of the week for which scheduling is to be done.
-- * Hours for each workday (9-5).
-- * Shift length constraints (minimum and maximum shift durations).
-- * A list of employees available for scheduling.
-- * Employer requirements for roles and their critical minimums.
--
module SchedulerContext (
    SchedulerContext(..)
) where

import Employee
import EmployerRequirements

-- Scheduler Context Type
data SchedulerContext = SchedulerContext {
    days :: [String],               -- The list of days for which scheduling is needed.
    hours :: [Int],                 -- The list of working hours for each day.
    shiftLengths :: (Int, Int),     -- Minimum and maximum shift lengths.
    employees :: [Employee],        -- List of employees available for scheduling.
    reqs :: EmployerRequirements    -- The employer's requirements.
} deriving (Show, Eq)
