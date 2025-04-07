-- | SchedulerCore.hs
-- 
-- This module contains the core scheduling logic for assigning employees to roles during specific work hours.
-- It defines the main solver function, which attempts to create a schedule that meets the employer's requirements
-- for staffing levels across various roles and time slots.
--
-- The module uses a recursive backtracking approach to try and fill the schedule, validating assignments and
-- ensuring that all critical minimum staffing levels are met. It also includes helper functions for managing the
-- state of the schedule and checking the availability of employees.
--
-- The module exposes the following functions:
--
-- * 'solveSchedule': The main function that recursively attempts to create a valid schedule.

module SchedulerCore (
    solveSchedule
) where

import Control.Monad (foldM)
import Employee
import EmployerRequirements
import SchedulerState
import SchedulerContext
import Validators (isValidAssignment, meetsCriticalMinimumsForDay)
import SchedulerOps (updateSchedule, removeAssignment)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Debug.Trace


-- Attempts to generate a valid schedule.
--
-- This function is the main scheduling solver. It recursively processes each day and hour to assign employees
-- to the required roles. If a valid schedule is found that meets all the employer's requirements, it returns
-- the updated scheduler state. If no valid schedule can be found, it returns 'Nothing'.
--
-- Arguments:
--   context: The 'SchedulerContext' that contains the configuration for the schedule, including days, hours, employees, and employer requirements.
--   state: The current state of the schedule being built.
--   index: The index of the day being processed.
--
-- Returns:
--   'Maybe SchedulerState': A valid schedule if found, or 'Nothing' if no valid schedule exists.
solveSchedule :: SchedulerContext -> SchedulerState -> Int -> Maybe SchedulerState
solveSchedule context state index
    | index >= length (days context) = Just state  -- Success case
    | otherwise =
        let day = days context !! index
            available = getAvailableEmployees context day
        in --trace ("\n--- Processing " ++ day ++ " ---") $
           processDay day context state available index


-- Processes one day of scheduling.
--
-- This function processes the schedule for one day. It iterates through each hour of the day and attempts to
-- assign the required roles to employees. If all roles are successfully assigned and the critical minimums are met,
-- it moves on to the next day. If an invalid schedule is encountered, it returns 'Nothing'.
--
-- Arguments:
--   day: The name of the day being processed.
--   context: The 'SchedulerContext' containing scheduling information.
--   state: The current 'SchedulerState' being updated.
--   available: A map of available employees by role for the day.
--   index: The index of the current day being processed.
--
-- Returns:
--   'Maybe SchedulerState': The updated state if the day was processed successfully, or 'Nothing'.
processDay :: String -> SchedulerContext -> SchedulerState -> Map.Map String [Employee] -> Int -> Maybe SchedulerState
processDay day context state available index =
    case foldM (\st hr -> processHour day hr context st available index) state (hours context) of
        Just updatedState | meetsCriticalMinimumsForDay (schedule updatedState) (reqs context) day ->
            solveSchedule context updatedState (index + 1)
        _ -> Nothing


-- Processes a single hour for a specific day.
--
-- This function processes a specific hour of a day and attempts to assign employees to the required roles. 
-- If a valid assignment is found for all roles at the given hour, it proceeds to the next hour. 
-- If any role cannot be assigned, it returns 'Nothing'.
--
-- Arguments:
--   day: The name of the day being processed.
--   hour: The hour being processed.
--   context: The 'SchedulerContext' containing scheduling information.
--   state: The current 'SchedulerState' being updated.
--   available: A map of available employees by role for the current hour.
--   index: The index of the current day being processed.
--
-- Returns:
--   'Maybe SchedulerState': The updated state after processing the hour, or 'Nothing'.
processHour :: String -> Int -> SchedulerContext -> SchedulerState -> Map.Map String [Employee] -> Int -> Maybe SchedulerState
processHour day hour context state available index =
    foldM (\st (role, req) -> processRole day hour role req context st available index) state (criticalMinimums (reqs context))


-- Processes a specific role for a specific hour of the day.
--
-- This function attempts to assign employees to a given role at a specific hour of the day. If the number of employees
-- assigned to the role meets the required minimum, it moves on. If the role is under-staffed, it attempts to assign employees.
--
-- Arguments:
--   day: The name of the day being processed.
--   hour: The hour being processed.
--   role: The name of the role being processed.
--   req: The required number of employees for the role.
--   context: The 'SchedulerContext' containing scheduling information.
--   state: The current 'SchedulerState' being updated.
--   available: A map of available employees by role.
--   index: The index of the current day being processed.
--
-- Returns:
--   'Maybe SchedulerState': The updated state after attempting to assign employees to the role.
processRole :: String -> Int -> String -> Int -> SchedulerContext -> SchedulerState -> Map.Map String [Employee] -> Int -> Maybe SchedulerState
processRole day hour role req context state available index =
    let current = getCurrentAssignments state day hour role
    in if current >= req 
        then Just state
        else tryEmployees (Map.findWithDefault [] role available) role day hour req context state index


-- Attempts to assign employees to a role at a specific time.
--
-- This function iterates through a list of available employees and attempts to assign them to the specified role
-- at the given time. If a valid assignment is found, it returns the updated state. If no valid assignment is found,
-- it continues with the next available employee.
--
-- Arguments:
--   employees: A list of available employees for the role.
--   role: The name of the role being processed.
--   day: The name of the day being processed.
--   hour: The hour being processed.
--   req: The required number of employees for the role.
--   context: The 'SchedulerContext' containing scheduling information.
--   state: The current 'SchedulerState' being updated.
--   index: The index of the current day being processed.
--
-- Returns:
--   'Maybe SchedulerState': The updated state if the assignment was successful, or 'Nothing'.
tryEmployees :: [Employee] -> String -> String -> Int -> Int -> SchedulerContext -> SchedulerState -> Int -> Maybe SchedulerState
tryEmployees [] _ _ _ _ _ _ _ = Nothing
tryEmployees (e:es) role day hour req context state index =
    case tryShifts e role day hour req context state index (EmployerRequirements.shiftLengths (reqs context)) of
        Just result -> Just result
        Nothing -> tryEmployees es role day hour req context state index


-- Attempts to assign a shift to an employee.
--
-- This function attempts to assign a shift to an employee and checks if the assignment meets the required constraints.
-- If the shift length is valid, the assignment is made, and the schedule is updated. If the assignment is not valid,
-- the function recursively tries the next shift length.
--
-- Arguments:
--   employee: The employee being considered for assignment.
--   role: The name of the role being processed.
--   day: The name of the day being processed.
--   hour: The hour being processed.
--   req: The required number of employees for the role.
--   context: The 'SchedulerContext' containing scheduling information.
--   state: The current 'SchedulerState' being updated.
--   index: The index of the current day being processed.
--   shiftLengths: The valid shift lengths.
--
-- Returns:
--   'Maybe SchedulerState': The updated state after trying to assign the shift, or 'Nothing'.
tryShifts :: Employee -> String -> String -> Int -> Int -> SchedulerContext -> SchedulerState -> Int -> (Int, Int) -> Maybe SchedulerState
tryShifts employee role day hour req context state index (minLen, maxLen)
    | shiftLen > maxLen = Nothing
    | start + shiftLen > snd (workHours (reqs context)) = 
        tryShifts employee role day hour req context state index (shiftLen + 1, maxLen)
    | isValidAssignment employee role day hour shiftLen
        (weeklyAssignedHours state) (dailyShifts state) (reqs context) (schedule state) = 
            let (newSched, newHours, newShifts) = updateSchedule employee role day hour shiftLen
                    (schedule state) (weeklyAssignedHours state) (dailyShifts state)
                newState = state { schedule = newSched
                                , weeklyAssignedHours = newHours
                                , dailyShifts = newShifts }
            in case solveSchedule context newState index of
                Just result -> Just result
                Nothing -> 
                    let (restoredSched, restoredHours, restoredShifts) = 
                            removeAssignment employee role day hour shiftLen newSched newHours newShifts
                        restoredState = state { schedule = restoredSched
                                             , weeklyAssignedHours = restoredHours
                                             , dailyShifts = restoredShifts }
                    in tryShifts employee role day hour req context restoredState index (shiftLen + 1, maxLen)
    | otherwise = tryShifts employee role day hour req context state index (shiftLen + 1, maxLen)
  where
    start = hour
    shiftLen = minLen



-- Gets the list of available employees for a specific day.
--
-- This helper function filters the list of employees based on their roles and days off, returning only those employees
-- who are available on the given day.
--
-- Arguments:
--   context: The 'SchedulerContext' containing employee data.
--   day: The day for which to retrieve available employees.
--
-- Returns:
--   A map of roles to the list of available employees for each role.
getAvailableEmployees :: SchedulerContext -> String -> Map.Map String [Employee]
getAvailableEmployees context day = Map.fromList
    [ (role, filter (\e -> Set.member role (roles e) && not (Set.member day (daysOff e))) 
        (employees context))
    | (role, _) <- criticalMinimums (reqs context) ]


-- Gets the current number of assignments for a specific role at a given hour of the day.
--
-- This function retrieves the current number of employees assigned to a role at a specific hour of the day.
--
-- Arguments:
--   state: The current 'SchedulerState' containing the current schedule.
--   day: The name of the day being processed.
--   hour: The hour being processed.
--   role: The role being processed.
--
-- Returns:
--   An integer representing the current number of employees assigned to the role at the given hour.
getCurrentAssignments :: SchedulerState -> String -> Int -> String -> Int
getCurrentAssignments state day hour role =
    maybe 0 length $ Map.lookup day (schedule state) >>= Map.lookup hour >>= Map.lookup role