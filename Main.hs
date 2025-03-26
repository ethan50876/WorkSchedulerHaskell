module Main where

import InputParser
import OutputWriter
import DataManager
import Schedule
import Data.Map as Map

main :: IO ()
main = do
    let employeePath = "data/employees.csv"
    let requirementsPath = "data/requirements.csv"

    employees <- parseEmployees employeePath
    requirements <- parseRequirements requirementsPath

    print (scheduleShifts employees requirements)

    echoEmployees employees
    echoRequirements requirements

scheduleShifts :: [Employee] -> EmployerRequirements -> Maybe ShiftSchedule
scheduleShifts emps reqs =
    let days = ["Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"]
        hours = [fst (workHours reqs) .. snd (workHours reqs)]
        shiftLengths = DataManager.shiftLengths reqs

        -- Intialize state with empty values
        schedule = Map.fromList $
                   Prelude.map (\day -> (day, Map.fromList $
                                 Prelude.map (\hour -> (hour, Map.empty)) hours)) days
        weeklyAssignedHours = Map.fromList $ Prelude.foldr (\emp acc -> (name emp, 0) : acc) [] emps
        dailyShifts = Map.empty

        context = SchedulerContext
                    days
                    hours
                    shiftLengths
                    emps
                    reqs
        state = SchedulerState
                  schedule
                  weeklyAssignedHours
                  dailyShifts

        result = solveSchedule context state 0

    in case result of
         Just schedule -> Just schedule
         Nothing -> Nothing
