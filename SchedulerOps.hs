-- SchedulerOps.hs
-- 
-- This module provides operations to update and remove assignments in the employee scheduling system.
-- It defines functions to assign employees to roles at specific hours and to remove employees from their shifts.
-- 
-- The module provides the following functions:
-- 
-- * 'updateSchedule': Assigns an employee to a role at specific hours and updates the schedule.
-- * 'removeAssignment': Removes an employee from their assigned role at specific hours and updates the schedule.
-- 
-- These operations modify the schedule, weekly assigned hours, and daily shifts.
-- 
module SchedulerOps (
    updateSchedule,
    removeAssignment
) where

import Employee
import ShiftSchedule
import qualified Data.Map as Map
import qualified Data.List as List
import qualified Data.Set as Set
import Debug.Trace (trace)

-- Updates the schedule by assigning an employee to a role at specified hours.
--
-- This function takes an employee, the role, the day, the starting hour, the shift length, and updates
-- the schedule by adding the employee to the specified role for the given hours. It also updates the
-- weekly assigned hours and daily shifts for the employee.
--
-- Arguments:
--   employee: The 'Employee' to be assigned.
--   role: The role to which the employee is being assigned.
--   day: The day on which the employee is assigned.
--   startHour: The starting hour of the shift.
--   shiftLength: The length of the shift (in hours).
--   schedule: The current shift schedule.
--   weeklyAssignedHours: A map of weekly assigned hours for each employee.
--   dailyShifts: A map of daily shifts for each employee.
--
-- Returns:
--   A tuple containing the updated schedule, weekly assigned hours, and daily shifts.
updateSchedule :: Employee -> String -> String -> Int -> Int -> ShiftSchedule -> Map.Map String Int -> Map.Map String [String] -> (ShiftSchedule, Map.Map String Int, Map.Map String [String])
updateSchedule employee role day startHour shiftLength schedule weeklyAssignedHours dailyShifts =
    let updatedSchedule = foldl (addShift employee role day) schedule [startHour..(startHour + shiftLength - 1)]
        updatedHours = Map.insertWith (+) (name employee) shiftLength weeklyAssignedHours
        updatedShifts = Map.insertWith (++) (name employee) [day] dailyShifts
    in trace ("Updated schedule with " ++ name employee ++ " for role " ++ role ++ " on " ++ day) (updatedSchedule, updatedHours, updatedShifts)


-- Adds a shift for an employee at a specific hour.
--
-- This function adds the employee to the schedule for the given role at a specified hour. It updates
-- the role schedule, the hour schedule, and the day schedule.
--
-- Arguments:
--   employee: The 'Employee' to be added.
--   role: The role to which the employee is being assigned.
--   day: The day of the assignment.
--   schedule: The current shift schedule.
--   hour: The hour at which the employee is assigned.
--
-- Returns:
--   The updated shift schedule after adding the employee.
addShift :: Employee -> String -> String -> ShiftSchedule -> Int -> ShiftSchedule
addShift employee role day schedule hour =
    trace ("Assigning " ++ name employee ++ " to role: " ++ role ++ " at hour: " ++ show hour ++ " on " ++ day) $
    let daySchedule = Map.findWithDefault Map.empty day schedule
        hourSchedule = Map.findWithDefault Map.empty hour daySchedule
        roleSchedule = Map.findWithDefault [] role hourSchedule
        updatedRoleSchedule = Map.insert role (employee : roleSchedule) hourSchedule
        updatedHourSchedule = Map.insert hour updatedRoleSchedule daySchedule
    in Map.insert day updatedHourSchedule schedule


-- Removes an employee's assignment from their role at specified hours.
--
-- This function removes the employee from the given role at specified hours in the schedule. It also updates
-- the weekly assigned hours and daily shifts, removing the day's assignment.
--
-- Arguments:
--   employee: The 'Employee' to be removed.
--   role: The role from which the employee is being removed.
--   day: The day on which the employee is removed.
--   startHour: The starting hour of the shift.
--   shiftLength: The length of the shift (in hours).
--   schedule: The current shift schedule.
--   weeklyAssignedHours: A map of weekly assigned hours for each employee.
--   dailyShifts: A map of daily shifts for each employee.
--
-- Returns:
--   A tuple containing the updated schedule, weekly assigned hours, and daily shifts.
removeAssignment :: Employee -> String -> String -> Int -> Int -> ShiftSchedule -> Map.Map String Int -> Map.Map String [String] -> (ShiftSchedule, Map.Map String Int, Map.Map String [String])
removeAssignment employee role day startHour shiftLength schedule weeklyAssignedHours dailyShifts =
    let updatedSchedule = foldl (removeShift employee role day) schedule [startHour..(startHour + shiftLength - 1)]
        updatedHours = Map.update (Just . subtract shiftLength) (name employee) weeklyAssignedHours
        updatedShifts = Map.adjust (filter (/= day)) (name employee) dailyShifts
    in trace ("Removing " ++ name employee ++ " from role " ++ role ++ " on " ++ day) (updatedSchedule, updatedHours, updatedShifts)


-- Removes a shift for an employee at a specific hour.
--
-- This function removes the employee from the schedule for the given role at a specific hour. It updates
-- the role schedule, the hour schedule, and the day schedule.
--
-- Arguments:
--   employee: The 'Employee' to be removed.
--   role: The role from which the employee is being removed.
--   day: The day of the shift being removed.
--   schedule: The current shift schedule.
--   hour: The hour at which the shift is being removed.
--
-- Returns:
--   The updated shift schedule after removing the employee's assignment.
removeShift :: Employee -> String -> String -> ShiftSchedule -> Int -> ShiftSchedule
removeShift employee role day schedule hour =
    trace ("Removing assignment of " ++ name employee ++ " from role: " ++ role ++ " at hour: " ++ show hour ++ " on " ++ day) $
    let daySchedule = Map.findWithDefault Map.empty day schedule
        hourSchedule = Map.findWithDefault Map.empty hour daySchedule
        roleSchedule = Map.findWithDefault [] role hourSchedule
        updatedRoleSchedule = Map.insert role (filter (/= employee) roleSchedule) hourSchedule
        updatedHourSchedule = Map.insert hour updatedRoleSchedule daySchedule
    in Map.insert day updatedHourSchedule schedule
