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

fillMinimumHrs :: SchedulerContext -> SchedulerState -> SchedulerState
fillMinimumHrs context state =
    let underMinHours = filter (\emp -> (weeklyAssignedHours state Map.! (name emp)) < (minHours emp)) (employees context)
        dayCounts = Map.map (sum . map length . Map.elems) (schedule state)
        role = "Floater"
    in foldl (processEmployee context dayCounts role) state underMinHours

processEmployee :: SchedulerContext -> Map.Map String Int -> String -> SchedulerState -> Employee -> SchedulerState
processEmployee context dayCounts role state employee =
    let requiredHours = minHours employee - (weeklyAssignedHours state Map.! name employee)
        sortedDays = filter (`notElem` daysOff employee) $ sortOn (\day -> Map.findWithDefault 0 day dayCounts) (Map.keys (schedule state))
        state' = assignExtraShifts employee role sortedDays requiredHours context state
    in if requiredHours > 0
        then extendExistingShifts employee role sortedDays requiredHours context state'
        else state'

assignExtraShifts :: Employee -> String -> [String] -> Int -> SchedulerContext -> SchedulerState -> SchedulerState
assignExtraShifts employee role sortedDays requiredHours context state = foldl assignDay state sortedDays
    where
        assignDay st day = foldl (assignHour day) st [fst (availability employee) .. snd (availability employee) - 1]
        
        assignHour day st hour
            | requiredHours <= 0 = st
            | otherwise =
                let shiftLength = snd (shiftLengths context)  
                    -- Validating from isValidAssignment
                    validAssignment = isValidAssignment employee role day hour shiftLength (weeklyAssignedHours st) (dailyShifts st) (reqs context) (schedule st)
                in if validAssignment
                   then let (newSchedule, newWeeklyHours, newDailyShifts) = updateSchedule employee role day hour shiftLength (schedule st) (weeklyAssignedHours st) (dailyShifts st)
                        in st { schedule = newSchedule, weeklyAssignedHours = newWeeklyHours, dailyShifts = newDailyShifts }
                   else st


extendExistingShifts :: Employee -> String -> [String] -> Int -> SchedulerContext -> SchedulerState -> SchedulerState
extendExistingShifts employee role sortedDays requiredHours context state =
                            foldl tryExtendShift state sortedDays
                          where
                            tryExtendShift st day
                                | requiredHours <= 0 || getShift (schedule st) employee day == (0,0) = st
                                | otherwise =
                                    let (start, end) = getShift (schedule st) employee day
                                        minStart = fst (availability employee)
                                        maxEnd = snd (availability employee)
                                        st' = extendEarlier st day start minStart
                                        st'' = extendLater st' day end maxEnd
                                    in st''
                            
                            extendEarlier st day start minStart
                                | start > minStart && requiredHours > 0 && (start - 1) >= minStart =
                                    let (newSchedule, newWeeklyHours, newDailyShifts) = updateSchedule employee role day (start - 1) 1 (schedule st) (weeklyAssignedHours st) (dailyShifts st)
                                    in extendEarlier (st { schedule = newSchedule, weeklyAssignedHours = newWeeklyHours, dailyShifts = newDailyShifts }) day (start - 1) minStart
                                | otherwise = st
                        
                            extendLater st day end maxEnd
                                | end < maxEnd && requiredHours > 0 && (end + 1) <= maxEnd =
                                    let (newSchedule, newWeeklyHours, newDailyShifts) = updateSchedule employee role day (end + 1) 1 (schedule st) (weeklyAssignedHours st) (dailyShifts st)
                                    in extendLater (st { schedule = newSchedule, weeklyAssignedHours = newWeeklyHours, dailyShifts = newDailyShifts }) day (end + 1) maxEnd
                                | otherwise = st

getShift :: Map.Map String (Map.Map Int (Map.Map String [Employee])) -> Employee -> String -> (Int, Int)
getShift schedule employee day =
    let shiftHours = [hour | (hour, roles) <- Map.toList (fromMaybe Map.empty (Map.lookup day schedule)),
                              any (elem employee) (Map.elems roles)]
    in if null shiftHours
       then (0, 0)  -- Return a default value if no shifts are found
       else (head shiftHours, last shiftHours)  -- Otherwise, return the first and last shift hours
