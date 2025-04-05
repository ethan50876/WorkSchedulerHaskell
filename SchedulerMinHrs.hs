module SchedulerMinHrs (
    fillMinimumHrs
) where

import SchedulerContext (SchedulerContext(..))
import SchedulerState (SchedulerState(..))
import Employee (Employee(..))
import SchedulerOps (updateSchedule)
import Validators

import Data.List (sortOn, delete)
import Data.Maybe (fromMaybe)
import Control.Monad (foldM) -- Add this import for foldM
import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Exception (try)

fillMinimumHrs :: SchedulerContext -> SchedulerState -> SchedulerState
fillMinimumHrs context state =
    let underMinHours = filter (\emp -> (weeklyAssignedHours state Map.! (name emp)) < (minHours emp)) (employees context)
        dayCounts = Map.map (sum . map length . Map.elems) (schedule state) --
        role = "Floater"
    in foldl (processEmployee context dayCounts role) state underMinHours

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

assignExtraShifts :: Employee -> String -> [String] -> Int -> SchedulerContext -> SchedulerState -> SchedulerState
assignExtraShifts _ _ sortedDays requiredHours context state |
    requiredHours < fst (shiftLengths context) || sortedDays == [] = state
assignExtraShifts employee role (day:days) requiredHours context state =
    let availableHours = [fst (availability employee) .. snd (availability employee) - 1
                                          - fst (shiftLengths context)]
        assignedState = tryAssignShift employee role day availableHours context state
        requiredHours' = minHours employee - (weeklyAssignedHours assignedState Map.! name employee)
    in assignExtraShifts employee role days requiredHours' context assignedState

tryAssignShift :: Employee -> String -> String -> [Int] -> SchedulerContext -> SchedulerState -> SchedulerState
tryAssignShift _ _ _ [] _ state = state
tryAssignShift employee role day (hour:hours) context state =
    if isValidAssignment employee role day hour shiftLen (weeklyAssignedHours state) (dailyShifts state) (reqs context) (schedule state)
                       then let (newSchedule, newWeeklyHours, newDailyShifts) = updateSchedule employee role day hour shiftLen (schedule state) (weeklyAssignedHours state) (dailyShifts state)
                            in state { schedule = newSchedule, weeklyAssignedHours = newWeeklyHours, dailyShifts = newDailyShifts }
                       else tryAssignShift employee role day hours context state

    where shiftLen = fst (shiftLengths context)

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

extendEarlier :: Employee -> String -> String -> Int -> Int -> Int -> SchedulerContext -> SchedulerState -> SchedulerState
extendEarlier employee day role requiredHours start end context state | 
    requiredHours <= 0 || start <= fst (availability employee) || end - start >= snd(shiftLengths context) = state
extendEarlier employee day role requiredHours start end context state =
    let newStart = start - 1
        extendedState = let (newSchedule, newWeeklyHours, newDailyShifts) = updateSchedule employee role day (newStart) 1 (schedule state) (weeklyAssignedHours state) (dailyShifts state)
                        in state { schedule = newSchedule, weeklyAssignedHours = newWeeklyHours, dailyShifts = newDailyShifts }
        requiredHours' = minHours employee - (weeklyAssignedHours extendedState Map.! name employee)
    in extendEarlier employee day role requiredHours' newStart end context extendedState

extendLater :: Employee -> String -> String -> Int -> Int -> Int -> SchedulerContext -> SchedulerState -> SchedulerState
extendLater employee day role requiredHours start end context state | 
    requiredHours <= 0 || end >= snd (availability employee) -1 || end - start >= snd(shiftLengths context) = state
extendLater employee day role requiredHours start end context state =
    let newEnd = end + 1
        extendedState = let (newSchedule, newWeeklyHours, newDailyShifts) = updateSchedule employee role day (newEnd) 1 (schedule state) (weeklyAssignedHours state) (dailyShifts state)
                        in state { schedule = newSchedule, weeklyAssignedHours = newWeeklyHours, dailyShifts = newDailyShifts }
        requiredHours' = minHours employee - (weeklyAssignedHours extendedState Map.! name employee)
    in extendLater employee day role requiredHours' start newEnd context extendedState

getShift :: Map.Map String (Map.Map Int (Map.Map String [Employee])) -> Employee -> String -> (Int, Int)
getShift schedule employee day =
    let shiftHours = [hour | (hour, roles) <- Map.toList (fromMaybe Map.empty (Map.lookup day schedule)),
                              any (elem employee) (Map.elems roles)]
    in if null shiftHours
       then (0, 0)  -- Return a default value if no shifts are found
       else (head shiftHours, last shiftHours)  -- Otherwise, return the first and last shift hours
