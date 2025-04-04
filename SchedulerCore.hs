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

-- Main solver function
solveSchedule :: SchedulerContext -> SchedulerState -> Int -> Maybe SchedulerState
solveSchedule context state index
    | index >= length (days context) = Just state  -- Success case
    | otherwise =
        let day = days context !! index
            available = getAvailableEmployees context day
        in trace ("\n--- Processing " ++ day ++ " ---") $
           processDay day context state available index

processDay :: String -> SchedulerContext -> SchedulerState -> Map.Map String [Employee] -> Int -> Maybe SchedulerState
processDay day context state available index =
    case foldM (\st hr -> processHour day hr context st available index) state (hours context) of
        Just updatedState | meetsCriticalMinimumsForDay (schedule updatedState) (reqs context) day ->
            solveSchedule context updatedState (index + 1)
        _ -> Nothing

processHour :: String -> Int -> SchedulerContext -> SchedulerState -> Map.Map String [Employee] -> Int -> Maybe SchedulerState
processHour day hour context state available index =
    foldM (\st (role, req) -> processRole day hour role req context st available index) state (criticalMinimums (reqs context))

processRole :: String -> Int -> String -> Int -> SchedulerContext -> SchedulerState -> Map.Map String [Employee] -> Int -> Maybe SchedulerState
processRole day hour role req context state available index =
    let current = getCurrentAssignments state day hour role
    in if current >= req 
        then Just state
        else tryEmployees (Map.findWithDefault [] role available) role day hour req context state index

tryEmployees :: [Employee] -> String -> String -> Int -> Int -> SchedulerContext -> SchedulerState -> Int -> Maybe SchedulerState
tryEmployees [] _ _ _ _ _ _ _ = Nothing
tryEmployees (e:es) role day hour req context state index =
    case tryShifts e role day hour req context state index (EmployerRequirements.shiftLengths (reqs context)) of
        Just result -> Just result
        Nothing -> tryEmployees es role day hour req context state index

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


getAvailableEmployees :: SchedulerContext -> String -> Map.Map String [Employee]
getAvailableEmployees context day = Map.fromList
    [ (role, filter (\e -> Set.member role (roles e) && not (Set.member day (daysOff e))) 
        (employees context))
    | (role, _) <- criticalMinimums (reqs context) ]

getCurrentAssignments :: SchedulerState -> String -> Int -> String -> Int
getCurrentAssignments state day hour role =
    maybe 0 length $ Map.lookup day (schedule state) >>= Map.lookup hour >>= Map.lookup role