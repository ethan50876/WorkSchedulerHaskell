module Schedule where
import Data.Map as Map
import DataManager

data SchedulerContext = SchedulerContext
  {
    days :: [String],
    hours :: [Int],
    shiftLengths :: (Int, Int),
    employees :: [Employee],
    reqs :: EmployerRequirements
  }

data SchedulerState = SchedulerState
  {
    schedule :: ShiftSchedule,
    weeklyAssignedHours :: WeeklyAssignedHours,
    dailyShifts :: DailyShifts
  }

type ShiftSchedule = Map String (Map Int (Map String [Employee]))
type WeeklyAssignedHours = Map String Int
type DailyShifts = Map String Int

solveSchedule :: SchedulerContext -> SchedulerState -> Int -> Maybe ShiftSchedule
solveSchedule context state index
    | index == length (days context) =
        if all (meetsCriticalMinimumsForDay (schedule state) (reqs context)) (days context)
        then Just (schedule state)
        else Nothing
    | otherwise =
        let day = days context !! index
            availableEmployees = Map.fromList
                [ (role, [e | e <- employees context, role `elem` roles e, day `notElem` daysOff e])
                | role <- Map.keys (criticalMinimums (reqs context))
                ]
        in case scheduleDay day context state availableEmployees index of
            Just newState -> solveSchedule context newState (index + 1)
            Nothing -> Nothing

meetsCriticalMinimumsForDay :: ShiftSchedule -> EmployerRequirements -> String -> Bool
meetsCriticalMinimumsForDay _ _ _ = False

scheduleDay :: String -> SchedulerContext -> SchedulerState -> Map String [Employee] -> Int -> Maybe SchedulerState
scheduleDay _ _ _ _ _ = Nothing