-- File Name:       Main.hs
-- Authors:         Ethan Ai, Kushal Saini
-- Couse:           COMP 3649 - 2025 Winter
-- Instructor:      Marc Schroeder
-- Assignment:      Functional Final Project: Work Shift Scheduler
--
--
-- This module is responsible for scheduling employee shifts based on parsed data from two .csv files:
-- one containing employee data and the other containing employer requirements.
--
-- The module performs the following tasks:
-- 
-- 1. **Parse Employee and Employer Data**: 
--    - The `scheduleShifts` function reads data from employee and employer requirement files, 
--      and creates the initial state for scheduling shifts.
-- 
-- 2. **Schedule Shifts**: 
--    - Using the `solveSchedule` function from the `SchedulerCore` module, it tries to schedule the employees
--      based on the parsed data and fills the schedule.
-- 
-- 3. **Print Schedule**: 
--    - If the schedule is generated successfully, the `printSchedule` function prints it to the console.
--
-- The module holds the following functions:
--
-- * 'scheduleShifts': Reads employee and employer requirements files and attempts to schedule shifts.
-- * 'printSchedule': Prints the generated schedule to the console.
-- * 'handleFileError': Handles file errors by printing a message and returning `Nothing`.
-- * 'main': The main driver that calls the scheduling functions and handles program execution.
module Main where

import Control.Monad (foldM)
import DataManager (parseEmployeeData, parseEmployerRequirements)
import Employee
import EmployerRequirements
import ShiftSchedule
import SchedulerState
import SchedulerContext
import SchedulerCore (solveSchedule)
import SchedulerMinHrs (fillMinimumHrs)
import qualified Data.Map as Map
import Control.Exception (catch, IOException)


-- Reads employee and employer requirements data, and attempts to schedule shifts.
--
-- This function reads two .csv files: one for employee data and one for employer requirements.
-- It initializes a scheduling context and state, then tries to solve the scheduling problem 
-- using the `solveSchedule` function from the `SchedulerCore` module.
-- The schedule is then finalized with the minimum hours using the `fillMinimumHrs` function.
-- If the schedule is successfully created, it is returned as a `Just ShiftSchedule`, otherwise, it returns `Nothing`.
--
-- Arguments:
--   employeeFile: The path to the .csv file containing employee data.
--   requirementsFile: The path to the .csv file containing employer requirements.
--
-- Returns:
--   An `IO` action that returns a `Maybe ShiftSchedule`, with `Just` if a valid schedule is generated,
--   or `Nothing` if there was an issue generating the schedule.
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
        Just state -> return $ Just finalSchedule
            where
                (SchedulerState finalSchedule _ _) = fillMinimumHrs context state
        Nothing -> return Nothing


-- Prints the generated shift schedule to the console.
--
-- This function iterates over the days of the week and prints the scheduled roles and their
-- assigned employees for each day. It handles cases where there is no assignment for a role at a specific hour.
--
-- Arguments:
--   schedule: The schedule to print, represented as a 'ShiftSchedule'.
--
-- Returns:
--   An `IO` action that prints the schedule to the console.
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
        let assignedStr = concatMap (++ ", ") assigned
        putStrLn $ show hour ++ ":00 - " ++ (if null assigned then "No Assignment" else init assignedStr)


-- Helper function to print file error messages.
handleFileError :: IOException -> IO (Maybe ShiftSchedule)
handleFileError _ = do
    putStrLn "Invalid file name or file not found."
    return Nothing


-- The main driver for the scheduling program.
--
-- This function coordinates the scheduling process by calling 'scheduleShifts' to generate a schedule
-- and 'printSchedule' to print the resulting schedule if one is found. 
--
-- Returns:
--   An `IO` action that runs the entire program, calling scheduling and printing functions.
main :: IO ()
main = do
    let employeeFile = "data/employees.csv"
        requirementsFile = "data/requirements.csv"
    schedule <- catch (scheduleShifts employeeFile requirementsFile)
                      handleFileError
    case schedule of
        Just s -> printSchedule s
        Nothing -> putStrLn "No valid schedule found."
