-- EmployerRequirements.hs
-- 
-- This module defines the 'EmployerRequirements' data type, which represents the requirements set by the employer
-- for employee work schedules. These requirements include work hours, shift lengths, and critical minimums for roles.
-- 
-- The 'EmployerRequirements' data type is used to store essential information for scheduling, such as:
-- * Work hours: The start and end times for the workday.
-- * Shift lengths: The minimum and maximum shift durations.
-- * Critical minimums: A list of critical roles with the required minimum number of employees.
--

module EmployerRequirements (
    EmployerRequirements(..)
) where

import qualified Data.Map as Map

-- Employer Requirements Data Type
data EmployerRequirements = EmployerRequirements {
    workHours :: (Int, Int),            -- The work hours (start and end time)
    shiftLengths :: (Int, Int),         -- The minimum and maximum shift lengths
    criticalMinimums :: [(String, Int)] -- Critical roles with their minimum number of employees
} deriving (Show, Eq)
