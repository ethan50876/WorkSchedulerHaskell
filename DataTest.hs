-- DataTest.hs
module DataTest where

import DataManager (parseEmployeeData, parseEmployerRequirements)
import Employee (Employee(..))
import EmployerRequirements (EmployerRequirements(..))
import qualified Data.Set as Set

main :: IO ()
main = do
    let employeeFile = "data/employees.csv"
        requirementsFile = "data/requirements.csv"

    -- Test Employee Data Parsing
    putStrLn "Testing Employee Data Parsing..."
    employees <- parseEmployeeData employeeFile
    mapM_ print employees

    -- Test Employer Requirements Parsing
    putStrLn "\nTesting Employer Requirements Parsing..."
    reqs <- parseEmployerRequirements requirementsFile
    print reqs

    putStrLn "\nData Checks:"
    putStrLn $ "Total Employees: " ++ show (length employees)
    putStrLn $ "Employee Names: " ++ show (map name employees)

    -- Extract roles from the list of tuples
    let criticalRoles = map fst (criticalMinimums reqs)
    putStrLn $ "Critical Minimum Roles: " ++ show criticalRoles

    let (startHour, endHour) = workHours reqs
    putStrLn $ "Work Hours: " ++ show startHour ++ " to " ++ show endHour

    let (minShift, maxShift) = shiftLengths reqs
    putStrLn $ "Shift Lengths: " ++ show minShift ++ " to " ++ show maxShift

    putStrLn "\nData Parsing Complete."
