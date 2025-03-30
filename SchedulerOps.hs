-- SchedulerOps.hs
module SchedulerOps (
    updateSchedule,
    removeAssignment
) where

import Employee (Employee(..))
import ShiftSchedule (ShiftSchedule(..))
import qualified Data.Map as Map
import qualified Data.List as List
import qualified Data.Set as Set
import Debug.Trace (trace)

-- Update Schedule
updateSchedule :: Employee -> String -> String -> Int -> Int -> ShiftSchedule -> Map.Map String Int -> Map.Map String [String] -> (ShiftSchedule, Map.Map String Int, Map.Map String [String])
updateSchedule employee role day startHour shiftLength schedule weeklyAssignedHours dailyShifts =
    let updatedSchedule = foldl (addShift employee role day) schedule [startHour..(startHour + shiftLength - 1)]
        updatedHours = Map.insertWith (+) (name employee) shiftLength weeklyAssignedHours
        updatedShifts = Map.insertWith (++) (name employee) [day] dailyShifts
    in trace ("Updated schedule with " ++ name employee ++ " for role " ++ role ++ " on " ++ day) (updatedSchedule, updatedHours, updatedShifts)

addShift :: Employee -> String -> String -> ShiftSchedule -> Int -> ShiftSchedule
addShift employee role day schedule hour =
    trace ("Assigning " ++ name employee ++ " to role: " ++ role ++ " at hour: " ++ show hour ++ " on " ++ day) $
    let daySchedule = Map.findWithDefault Map.empty day schedule
        hourSchedule = Map.findWithDefault Map.empty hour daySchedule
        roleSchedule = Map.findWithDefault [] role hourSchedule
        updatedRoleSchedule = Map.insert role (employee : roleSchedule) hourSchedule
        updatedHourSchedule = Map.insert hour updatedRoleSchedule daySchedule
    in Map.insert day updatedHourSchedule schedule

-- Remove Assignment
removeAssignment :: Employee -> String -> String -> Int -> Int -> ShiftSchedule -> Map.Map String Int -> Map.Map String [String] -> (ShiftSchedule, Map.Map String Int, Map.Map String [String])
removeAssignment employee role day startHour shiftLength schedule weeklyAssignedHours dailyShifts =
    let updatedSchedule = foldl (removeShift employee role day) schedule [startHour..(startHour + shiftLength - 1)]
        updatedHours = Map.update (Just . subtract shiftLength) (name employee) weeklyAssignedHours
        updatedShifts = Map.adjust (filter (/= day)) (name employee) dailyShifts
    in trace ("Removing " ++ name employee ++ " from role " ++ role ++ " on " ++ day) (updatedSchedule, updatedHours, updatedShifts)

removeShift :: Employee -> String -> String -> ShiftSchedule -> Int -> ShiftSchedule
removeShift employee role day schedule hour =
    trace ("Removing assignment of " ++ name employee ++ " from role: " ++ role ++ " at hour: " ++ show hour ++ " on " ++ day) $
    let daySchedule = Map.findWithDefault Map.empty day schedule
        hourSchedule = Map.findWithDefault Map.empty hour daySchedule
        roleSchedule = Map.findWithDefault [] role hourSchedule
        updatedRoleSchedule = Map.insert role (filter (/= employee) roleSchedule) hourSchedule
        updatedHourSchedule = Map.insert hour updatedRoleSchedule daySchedule
    in Map.insert day updatedHourSchedule schedule
