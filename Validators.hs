-- Validators.hs
module Validators (
    isValidAssignment,
    meetsCriticalMinimumsForDay,
    hasWorkedToday
) where

import Employee (Employee(..))
import EmployerRequirements (EmployerRequirements(..))
import ShiftSchedule (ShiftSchedule)
import qualified Data.Map as Map
import qualified Data.Set as Set

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


roleCount :: String -> Map.Map Int (Map.Map String [Employee]) -> Int
roleCount role daySchedule =
    sum $ map (length . Map.findWithDefault [] role) (Map.elems daySchedule)

meetsCriticalMinimumsForDay :: ShiftSchedule -> EmployerRequirements -> String -> Bool
meetsCriticalMinimumsForDay schedule reqs day =
    let daySchedule = Map.findWithDefault Map.empty day schedule
        missingRoles = filter (\(role, minReq) -> roleCount role daySchedule < minReq) (criticalMinimums reqs)
    in null missingRoles

hasWorkedToday :: Employee -> String -> ShiftSchedule -> Bool
hasWorkedToday employee day schedule =
    case Map.lookup day schedule of
        Just daySchedule -> any (any (elem employee) . Map.elems) (Map.elems daySchedule)
        Nothing -> False

