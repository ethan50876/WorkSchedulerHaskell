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
import SchedulerMinHrs (fillMinimumHrs)
import qualified Data.Map as Map
import Control.Exception (catch, IOException)

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
        ---Just (SchedulerState s _ _) -> return $ Just s
        Just state -> return $ Just finalSchedule
            where
                (SchedulerState finalSchedule _ _) = fillMinimumHrs context state
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


handleFileError :: IOException -> IO (Maybe ShiftSchedule)
handleFileError _ = do
    putStrLn "Invalid file name or file not found."
    return Nothing

main :: IO ()
main = do
    let employeeFile = "data/employees.csv"
        requirementsFile = "data/requirements.csv"
    schedule <- catch (scheduleShifts employeeFile requirementsFile)
                      handleFileError
    case schedule of
        Just s -> printSchedule s
        Nothing -> putStrLn "No valid schedule found."
