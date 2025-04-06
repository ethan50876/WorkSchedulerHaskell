-- Validators.hs
--
-- This module defines validation functions used to check and validate possible employee assignments.
-- The module contains functions that ensure employee assignments satisfy various constraints such as work hours, 
-- availability, role requirements, and critical staffing levels.
--
-- The module provides the following functions:
-- 
-- * 'isValidAssignment': Checks if an employee can be assigned to a specific role at a given time.
-- * 'meetsCriticalMinimumsForDay': Ensures that the minimum staffing requirements are met for all roles on a given day.
-- * 'hasWorkedToday': Checks if an employee has already been assigned to any shift on a specific day.
module Validators (
    isValidAssignment,
    meetsCriticalMinimumsForDay,
    hasWorkedToday
) where

import Employee
import EmployerRequirements
import ShiftSchedule
import qualified Data.Map as Map
import qualified Data.Set as Set

-- Checks if an employee can be assigned to a role at a specific time.
--
-- This function checks if the employee can be assigned to a role at the given start hour and shift length.
-- It validates whether the assignment exceeds the employee's weekly hours, whether the employee is on their day off,
-- if the shift is within their availability, and whether the role matches one of the employee's assigned roles.
--
-- Arguments:
--   employee: The employee being considered for assignment.
--   role: The role to be assigned to the employee.
--   day: The day of the assignment.
--   startHour: The start hour of the shift.
--   shiftLength: The length of the shift (in hours).
--   weeklyAssignedHours: A map of employee names to their weekly assigned hours.
--   dailyShifts: A map of employee names to their daily shifts.
--   reqs: The employer's requirements for staffing levels.
--   schedule: The current shift schedule.
--
-- Returns:
--   A boolean indicating whether the assignment is valid.
isValidAssignment :: Employee -> String -> String -> Int -> Int -> Map.Map String Int -> Map.Map String [String] -> EmployerRequirements -> ShiftSchedule -> Bool
isValidAssignment employee role day startHour shiftLength weeklyAssignedHours dailyShifts reqs schedule =
    let currentHours = Map.findWithDefault 0 (name employee) weeklyAssignedHours
        dayShifts = Map.findWithDefault [] (name employee) dailyShifts
        exceedsHours = currentHours + shiftLength > maxHours employee
        onDayOff = Set.member day (daysOff employee) 
        outsideAvailability = startHour < fst (availability employee) || startHour + shiftLength > snd (availability employee)
        roleMismatch = not ( (Set.member role (roles employee))  || (role == "Floater") )
        hasWorked = hasWorkedToday employee day schedule
    in not (exceedsHours || onDayOff || outsideAvailability || roleMismatch || hasWorked)


-- Helper function to count the number of employees assigned to a role on a given day
--
-- Arguments:
--   role: The role for which to count the assigned employees.
--   daySchedule: The schedule for the given day.
--
-- Returns:
--   An integer representing the number of employees assigned to the given role for the day.
roleCount :: String -> Map.Map Int (Map.Map String [Employee]) -> Int
roleCount role daySchedule =
    sum $ map (length . Map.findWithDefault [] role) (Map.elems daySchedule)


-- Checks if the staffing requirements for all roles are met on a specific day.
--
-- This function ensures that the minimum staffing requirements for all roles are met for the given day.
-- It checks if any role has fewer employees than the required minimums as specified by the employer.
--
-- Arguments:
--   schedule: The shift schedule.
--   reqs: The employer's requirements.
--   day: The day to check for critical staffing requirements.
--
-- Returns:
--   A boolean indicating whether the critical staffing requirements are met for the day.
meetsCriticalMinimumsForDay :: ShiftSchedule -> EmployerRequirements -> String -> Bool
meetsCriticalMinimumsForDay schedule reqs day =
    let daySchedule = Map.findWithDefault Map.empty day schedule
        missingRoles = filter (\(role, minReq) -> roleCount role daySchedule < minReq) (criticalMinimums reqs)
    in null missingRoles


-- Checks if an employee has worked on a specific day.
--
-- This function checks if the employee has been assigned to any shift on the given day.
--
-- Arguments:
--   employee: The employee to check.
--   day: The day to check if the employee has worked.
--   schedule: The current shift schedule.
--
-- Returns:
--   A boolean indicating whether the employee has already worked on the specified day.
hasWorkedToday :: Employee -> String -> ShiftSchedule -> Bool
hasWorkedToday employee day schedule =
    case Map.lookup day schedule of
        Just daySchedule -> any (any (elem employee) . Map.elems) (Map.elems daySchedule)
        Nothing -> False

