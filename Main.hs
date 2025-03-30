-- Main.hs
module Main where

import Control.Monad (foldM)
import DataManager (parseEmployeeData, parseEmployerRequirements)
import Employee (Employee(..))
import EmployerRequirements (EmployerRequirements(..))
import ShiftSchedule (ShiftSchedule)
import SchedulerState (SchedulerState(..))
import SchedulerContext (SchedulerContext(..))
import SchedulerCore (solveSchedule)
import qualified Data.Map as Map

scheduleShifts :: FilePath -> FilePath -> IO (Maybe ShiftSchedule)
scheduleShifts employeeFile requirementsFile = do
    employees <- parseEmployeeData employeeFile
    reqs <- parseEmployerRequirements requirementsFile

    let days = ["Monday", "Tuesday", "Wednesday", "Thursday", "Friday"]
        hours = [(fst $ workHours reqs)..(snd (workHours reqs) - 1)]
        shiftRange = EmployerRequirements.shiftLengths reqs
        schedule = Map.fromList [(day, Map.fromList [(h, Map.empty) | h <- hours]) | day <- days]
        weeklyAssignedHours = Map.fromList [(name e, 0) | e <- employees]
        dailyShifts = Map.empty
        context = SchedulerContext days hours shiftRange employees reqs
        initialState = SchedulerState schedule weeklyAssignedHours dailyShifts

    case solveSchedule context initialState 0 of
        Just (SchedulerState s _ _) -> return $ Just s
        Nothing -> return Nothing

printSchedule :: ShiftSchedule -> IO ()
printSchedule schedule = mapM_ printDay days
  where
    days = ["Monday", "Tuesday", "Wednesday", "Thursday", "Friday"]
    
    printDay day = case Map.lookup day schedule of
        Just hours -> do
            putStrLn $ day ++ ":"
            mapM_ printHour (Map.toList hours)
        Nothing -> putStrLn $ day ++ ": No Schedule Found"
    
    printHour (hour, roles) = do
        let assigned = ["(" ++ role ++ ") " ++ name e | (role, employees) <- Map.toList roles, e <- employees]
        putStrLn $ show hour ++ ":00 - " ++ (if null assigned then "No Assignment" else unwords assigned)

main :: IO ()
main = do
    let employeeFile = "data/employees.csv"
        requirementsFile = "data/requirements.csv"
    schedule <- scheduleShifts employeeFile requirementsFile
    case schedule of
        Just s -> printSchedule s
        Nothing -> putStrLn "No valid schedule found."