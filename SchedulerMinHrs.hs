-- SchedulerMinHrs.hs
--
-- This module contains the functionality to fill the minimum hours for employees who are under their required hours.
-- It processes each employee, checking their assigned hours and attempting to assign extra shifts or extend existing shifts
-- to meet their minimum hour requirements.
--
-- Limitations: There are cases where the function may not be able to fill the minimum hours for an employee due to
-- the constraints of the employee's availability, days off, and the maximum allowed shift length. In such cases, 
-- a schedule will still be returned with the best effort to meet the minimum hours.
--
-- The module exposes the following function:
--
-- 'fillMinimumHrs': The main function that serves as the interface for this module.

module SchedulerMinHrs (
    fillMinimumHrs
) where

import SchedulerContext
import SchedulerState
import Employee
import SchedulerOps
import Validators

import Data.List (sortOn, delete)
import Data.Maybe (fromMaybe)
import Control.Monad (foldM) -- Add this import for foldM
import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Exception (try)

{-|
    The 'fillMinimumHrs' function ensures that employees in the scheduling system
    are assigned at least their minimum required hours. It takes the current
    scheduling context and state, and returns an updated state with adjustments
    made to meet the minimum hour requirements.

    Arguements:
        - 'context' ('SchedulerContext'): The context containing information about
            employees, roles, and other scheduling constraints.
        - 'state' ('SchedulerState'): The current state of the schedule, including
            assigned hours and daily schedules.

    Returns:
        - An updated 'SchedulerState' with adjustments made to ensure all employees
            meet their minimum required hours.
-}
fillMinimumHrs :: SchedulerContext -> SchedulerState -> SchedulerState
fillMinimumHrs context state =
    let underMinHours = filter (\emp -> (weeklyAssignedHours state Map.! (name emp)) < (minHours emp)) (employees context)
        dayCounts = Map.map (sum . map length . Map.elems) (schedule state) --
        role = "Floater"
    in foldl (processEmployee context dayCounts role) state underMinHours


{-|
The 'processEmployee' function is responsible for scheduling extra shifts for a given employee who is under their minimum hours.

Arguements:
    * 'context' - The 'SchedulerContext' containing global scheduling information and constraints.
    * 'dayCounts' - A map from day names to the number of shifts already assigned on those days.
    * 'role' - The role or position the employee is being scheduled for.
    * 'state' - The current 'SchedulerState', which includes the schedule and other stateful information.
    * 'employee' - The 'Employee' being processed for scheduling.

Return:
    * An updated 'SchedulerState' after attempting to assign shifts to the employee.

Notes: 
    * The function first calculates the required hours for the employee based on their current assigned hours.
    * It then filters the days to find those that are not in the employee's days off and sorts them based on the number of shifts already assigned.
    * The function will attempt to assign extra shifts to the employee on these sorted days until their required hours are met or no more shifts can be assigned.
    * If there are still required hours left, it will try to extend existing shifts for the employee on days where they have already been assigned.
-}
processEmployee :: SchedulerContext -> Map.Map String Int -> String -> SchedulerState -> Employee -> SchedulerState
processEmployee context dayCounts role state employee =
    let requiredHours = minHours employee - (weeklyAssignedHours state Map.! name employee)
        sortedDays = filter (`notElem` daysOff employee) $ sortOn (\day -> Map.findWithDefault 0 day dayCounts) (Map.keys (schedule state))
        state' = assignExtraShifts employee role sortedDays requiredHours context state
        requiredHours' = minHours employee - (weeklyAssignedHours state' Map.! name employee)
        extendableDays = filter (\day -> any (employee `elem`) (concatMap Map.elems (Map.elems (fromMaybe Map.empty (Map.lookup day (schedule state)))))) (Map.keys (schedule state))
    in if requiredHours' > 0
        then extendExistingShifts employee role extendableDays requiredHours context state'
        else state'


{-|
    The 'assignExtraShifts' function is responsible for assigning additional shifts
    to an employee until their required minimum weekly hours are met or no more
    shifts can be assigned.

    Arguements:
        * 'Employee' - The employee to whom extra shifts are being assigned.
        * 'String' - The role of the employee for the shifts.
        * '[String]' - A list of sorted days (e.g., ["Monday", "Tuesday", ...]) on which shifts can be assigned.
        * 'Int' - The number of required hours that the employee still needs to fulfill.
        * 'SchedulerContext' - The context containing scheduling rules and constraints, such as shift lengths.
        * 'SchedulerState' - The current state of the scheduler, including assigned shifts.

    Returns:
        * 'SchedulerState' - The updated scheduler state after attempting to assign extra shifts.

    Notes:
        * If the required hours are less than the minimum shift length or there are no more days to assign shifts, 
            the function returns the current state without making any changes.
        * Otherwise, it recursively iterates through the sorted days, attempting to assign shifts to the employee
            based on their availability and the scheduling constraints.
        * After each assignment, it recalculates the remaining required hours and continues assigning shifts
            until the required hours are met or all days are exhausted.
-}

assignExtraShifts :: Employee -> String -> [String] -> Int -> SchedulerContext -> SchedulerState -> SchedulerState
assignExtraShifts _ _ sortedDays requiredHours context state |
    requiredHours < fst (shiftLengths context) || sortedDays == [] = state
assignExtraShifts employee role (day:days) requiredHours context state =
    let availableHours = [fst (availability employee) .. snd (availability employee) - 1
                                          - fst (shiftLengths context)]
        assignedState = tryAssignShift employee role day availableHours context state
        requiredHours' = minHours employee - (weeklyAssignedHours assignedState Map.! name employee)
    in assignExtraShifts employee role days requiredHours' context assignedState

{-|
    The 'tryAssignShift' function attempts to assign a shift to an employee based on the provided parameters.
    It recursively tries to assign the shift for each hour in the given list of hours until a valid assignment is found
    or the list is exhausted.

    Arguements:
        * 'employee' - The employee to whom the shift is being assigned.
        * 'role' - The role the employee will perform during the shift.
        * 'day' - The day on which the shift is being assigned.
        * '[Int]' - A list of hours representing potential start times for the shift.
        * 'context' - The 'SchedulerContext' containing configuration and constraints for scheduling.
        * 'state' - The current 'SchedulerState' representing the state of the schedule.

    Returns:
        * A new 'SchedulerState' with the updated schedule if a valid assignment is made, or the original state if no valid assignment is possible.

    Notes:
        * The function checks if the assignment is valid using the 'isValidAssignment' function.
        * If valid, it updates the schedule, weekly assigned hours, and daily shifts using the 'updateSchedule' function.
        * If no valid assignment is found for the current hour, it recursively tries the next hour in the list.
        * The 'shiftLen' is determined from the 'shiftLengths' field in the 'SchedulerContext'.
-}
tryAssignShift :: Employee -> String -> String -> [Int] -> SchedulerContext -> SchedulerState -> SchedulerState
tryAssignShift _ _ _ [] _ state = state
tryAssignShift employee role day (hour:hours) context state =
    if isValidAssignment employee role day hour shiftLen (weeklyAssignedHours state) (dailyShifts state) (reqs context) (schedule state)
                       then let (newSchedule, newWeeklyHours, newDailyShifts) = updateSchedule employee role day hour shiftLen (schedule state) (weeklyAssignedHours state) (dailyShifts state)
                            in state { schedule = newSchedule, weeklyAssignedHours = newWeeklyHours, dailyShifts = newDailyShifts }
                       else tryAssignShift employee role day hours context state

    where shiftLen = fst (shiftLengths context)

{-|
    The `extendExistingShifts` function attempts to extend the length of the employee's
    existing shifts to meet the required minimum hours. It does so by iterating
    through the provided list of days where the employee has a shift and adjusting the employee's shift accordingly.

    Arguements:
        - `employee :: Employee`: The employee whose shifts are being extended.
        - `role :: String`: The role or position of the employee.
        - `days :: [String]`: The list of days where the employee already has a shift.
        - `requiredHours :: Int`: The number of hours that need to be added to meet the minimum requirement.
        - `context :: SchedulerContext`: The context containing scheduling rules and constraints.
        - `state :: SchedulerState`: The current state of the scheduler, including the schedule and other metadata.

    Returns:
        - `SchedulerState`: The updated scheduler state after attempting to extend the shifts.

    Note:
        - The function will recursively call itself until the required hours are met or there are no more days to process.
        - It first attempts to ajust the shift to start earlier and then later, if needed.
        - If the required hours are already met or if there are no more days to process, it returns the current state.
-}
extendExistingShifts :: Employee -> String -> [String] -> Int -> SchedulerContext -> SchedulerState -> SchedulerState
extendExistingShifts _ _ days requiredHours _ state |
    requiredHours <= 0 || days == [] = state
extendExistingShifts employee role (day:days) requiredHours context state =
    let (startHour, endHour) = getShift (schedule state) employee day
        extendedState = extendEarlier employee day role requiredHours startHour endHour context state
        requiredHours' = minHours employee - (weeklyAssignedHours extendedState Map.! name employee)
        startHour' = fst (getShift (schedule extendedState) employee day)
        extendedState' = extendLater employee day role requiredHours' startHour' endHour context extendedState
        requiredHours'' = minHours employee - (weeklyAssignedHours extendedState' Map.! name employee)
    in if requiredHours'' > 0
       then extendExistingShifts employee role days requiredHours'' context extendedState'
       else extendedState'


{-|
The 'extendEarlier' function attempts to extend an employee's work schedule earlier in the day
to meet the required minimum hours while adhering to constraints such as availability and shift lengths.

Parameters:
    * 'employee' - The 'Employee' whose schedule is being extended.
    * 'day' - The day of the week (as a 'String') for which the schedule is being modified.
    * 'role' - The role (as a 'String') the employee is assigned to during the shift.
    * 'requiredHours' - The number of hours still required to meet the employee's minimum hours.
    * 'start' - The current start time of the shift being extended.
    * 'end' - The end time of the shift.
    * 'context' - The 'SchedulerContext' containing scheduling constraints and rules.
    * 'state' - The current 'SchedulerState' representing the overall scheduling state.

Returns:
    * A new 'SchedulerState' with the updated schedule after attempting to extend the shift earlier.

Notes:
    * The function checks if the required hours are less than or equal to zero, if the start time is before the employee's availability,
      or if the difference between the end and start times is greater than or equal to the maximum shift length allowed.
    * If any of these conditions are met, it returns the current state without making changes.
    * Otherwise, it recursively calls itself to extend the shift earlier by one hour until the required hours are met or constraints are violated.
-}
extendEarlier :: Employee -> String -> String -> Int -> Int -> Int -> SchedulerContext -> SchedulerState -> SchedulerState
extendEarlier employee day role requiredHours start end context state | 
    requiredHours <= 0 || start <= fst (availability employee) || end - start >= snd(shiftLengths context) = state
extendEarlier employee day role requiredHours start end context state =
    let newStart = start - 1
        extendedState = let (newSchedule, newWeeklyHours, newDailyShifts) = updateSchedule employee role day (newStart) 1 (schedule state) (weeklyAssignedHours state) (dailyShifts state)
                        in state { schedule = newSchedule, weeklyAssignedHours = newWeeklyHours, dailyShifts = newDailyShifts }
        requiredHours' = minHours employee - (weeklyAssignedHours extendedState Map.! name employee)
    in extendEarlier employee day role requiredHours' newStart end context extendedState


{-|
The 'extendLater' function attempts to extend an employee's work schedule laster in the day
to meet the required minimum hours while adhering to constraints such as availability and shift lengths.

Parameters:
    * 'employee' - The 'Employee' whose schedule is being extended.
    * 'day' - The day of the week (as a 'String') for which the schedule is being modified.
    * 'role' - The role (as a 'String') the employee is assigned to during the shift.
    * 'requiredHours' - The number of hours still required to meet the employee's minimum hours.
    * 'start' - The start time of the shift.
    * 'end' - The current end time of the shift being extended.
    * 'context' - The 'SchedulerContext' containing scheduling constraints and rules.
    * 'state' - The current 'SchedulerState' representing the overall scheduling state.

Returns:
    * A new 'SchedulerState' with the updated schedule, weekly assigned hours, 
        and daily shifts after attempting to extend the employee's shift.

Notes:
    * The function checks if the required hours are less than or equal to zero, if the end time is after the employee's availability,
      or if the difference between the end and start times is greater than or equal to the maximum shift length allowed.
    * If any of these conditions are met, it returns the current state without making changes.
    * Otherwise, it recursively calls itself to extend the shift later by one hour until the required hours are met or constraints are violated.
-}
extendLater :: Employee -> String -> String -> Int -> Int -> Int -> SchedulerContext -> SchedulerState -> SchedulerState
extendLater employee day role requiredHours start end context state | 
    requiredHours <= 0 || end >= snd (availability employee) -1 || end - start >= snd(shiftLengths context) = state
extendLater employee day role requiredHours start end context state =
    let newEnd = end + 1
        extendedState = let (newSchedule, newWeeklyHours, newDailyShifts) = updateSchedule employee role day (newEnd) 1 (schedule state) (weeklyAssignedHours state) (dailyShifts state)
                        in state { schedule = newSchedule, weeklyAssignedHours = newWeeklyHours, dailyShifts = newDailyShifts }
        requiredHours' = minHours employee - (weeklyAssignedHours extendedState Map.! name employee)
    in extendLater employee day role requiredHours' start newEnd context extendedState


{-|
The 'getShift' function retrieves the shift start and end times of a specific employee on a given day from a nested schedule map.

Parameters:
    * 'schedule': A nested map structure where:
            - The outer map's keys are days (as 'String').
            - The middle map's keys are hours (as 'Int').
            - The inner map's keys are roles (as 'String'), and the values are lists of employees ('[Employee]') assigned to those roles.
    * 'employee': The 'Employee' whose shift hours need to be determined.
    * 'day': The day (as 'String') for which the shift hours are being queried.

Returns:
    A tuple '(Int, Int)' representing:
        - The starting hour of the employee's shift.
        - The ending hour of the employee's shift.
    If the employee is not scheduled for any shift on the given day, the function returns '(0, 0)' as a default value.

Notes:
    * The function uses 'Map.lookup' to find the schedule for the specified day.
    * If the day is not found or the employee is not assigned to any role, the function returns the default value.
    * If the employee is scheduled, the function determins the earliest and latest hours from the list of hours where the employee is assigned.
-}
getShift :: Map.Map String (Map.Map Int (Map.Map String [Employee])) -> Employee -> String -> (Int, Int)
getShift schedule employee day =
    let shiftHours = [hour | (hour, roles) <- Map.toList (fromMaybe Map.empty (Map.lookup day schedule)),
                              any (elem employee) (Map.elems roles)]
    in if null shiftHours
       then (0, 0)  -- Return a default value if no shifts are found
       else (head shiftHours, last shiftHours)  -- Otherwise, return the first and last shift hours
