module OutputWriter (
    echoEmployees,
    echoRequirements
) where

import Employee
import EmployerRequirements
import qualified Data.Map as Map
import qualified Data.Set as Set

-- Print each employee nicely
echoEmployees :: [Employee] -> IO ()
echoEmployees emps = do
    putStrLn "\nParsed Employees:"
    mapM_ printEmployee emps

printEmployee :: Employee -> IO ()
printEmployee (Employee name (start, end) minH maxH roles daysOff) = do
    putStrLn $ "- " ++ name ++ " (" ++ show start ++ ":00 to " ++ show end ++ ":00)"
    putStrLn $ "  Min Hours: " ++ show minH ++ ", Max Hours: " ++ show maxH
    putStrLn $ "  Roles: " ++ show (Set.toList roles)
    putStrLn $ "  Days Off: " ++ show (Set.toList daysOff)

echoRequirements :: EmployerRequirements -> IO ()
echoRequirements (EmployerRequirements (open, close) (minShift, maxShift) critMin) = do
    putStrLn "\nEmployer Requirements:"
    putStrLn $ "  Work Hours: " ++ show open ++ ":00 - " ++ show close ++ ":00"
    putStrLn $ "  Shift Lengths: " ++ show minShift ++ " - " ++ show maxShift
    putStrLn "  Critical Minimums:"
    mapM_ (\(r, m) -> putStrLn $ "    - " ++ r ++ ": " ++ show m) (Map.toList critMin)
