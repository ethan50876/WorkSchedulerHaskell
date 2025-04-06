-- | DataManager.hs
-- 
-- This module provides functions for parsing employee and employer requirements data from CSV files.
-- It includes functions to parse employee information, including their roles, availability, minimum and
-- maximum working hours, and days off. Additionally, it provides a function to parse employer requirements
-- that specify the work hours, shift lengths, and critical role requirements.
-- 
-- The module exposes the following functions:
-- 
-- * 'parseEmployeeData': Parses a CSV file to retrieve employee data.
-- * 'parseEmployerRequirements': Parses a CSV file to retrieve employer requirements.
-- * 'insertInOrder': Inserts a key-value pair into a list of key-value pairs in order.
-- * 'parseHours': Parses a string representing hours (ex."9-18") into a tuple of integers.
-- * Utility functions for parsing and manipulating CSV data (ex. splitting by comma).
module DataManager (
    parseEmployeeData,
    parseEmployerRequirements
) where

import System.IO
import qualified Data.Set as Set
import qualified Data.Map as Map
import Employee
import EmployerRequirements

-- Parses the employee data from a .csv file.
--
-- This function reads a .csv file containing employee information and returns a list of 'Employee' objects.
-- The file is expected to have rows representing each employee's details, including their name, availability,
-- minimum and maximum working hours, roles, and days off.
--
-- Arguments:
--   filePath: The path to the employee data .csv file.
--
-- Returns:
--   A list of 'Employee' objects parsed from the .csv file.
parseEmployeeData :: FilePath -> IO [Employee]
parseEmployeeData filePath = do
    content <- readFile filePath
    let rows = tail $ lines content
    return $ map parseEmployee rows


-- Parses a single row of employee data.
--
-- This function converts a row of employee data into an instance of the 'Employee' object. It expects a .csv where the 
-- row contains the following columns: name, availability, minimum hours, maximum hours, roles, and days off.
--
-- Arguments:
--   row: A string representing a single row of employee data in .csv format.
--
-- Returns:
--   An 'Employee' object parsed from the row data.
parseEmployee :: String -> Employee
parseEmployee row =
    case splitCSV row of
        [name, availability, minH, maxH, rolesStr, daysOffStr] ->
            let cleanedRoles = splitComma (stripQuotes rolesStr)
                cleanedDaysOff = splitSemi (stripQuotes daysOffStr)
            in Employee {
                name = stripQuotes name,
                availability = parseHours availability,
                minHours = read minH,
                maxHours = read maxH,
                roles = createRoleSet cleanedRoles,
                daysOff = createDaysOffSet cleanedDaysOff
            }
        _ -> error $ "Invalid employee row: " ++ row


-- Parses the employer requirements data from a .csv file.
--
-- This function reads a C,csv file containing employer requirements for scheduling and returns an 'EmployerRequirements'
-- object that specifies the work hours, shift lengths, and critical minimum requirements for various roles.
--
-- Arguments:
--   filePath: The path to the employer requirements .csv file.
--
-- Returns:
--   An 'EmployerRequirements' object parsed from the .csv file.
parseEmployerRequirements :: FilePath -> IO EmployerRequirements
parseEmployerRequirements filePath = do
    content <- readFile filePath
    let rows = tail $ lines content
        parsedRows = map splitCSV rows
    if all (\r -> length r == 4) parsedRows then
        let criticalMinimums = [(stripQuotes role, read minReq) | [role, minReq, _, _] <- parsedRows]
            workHours = parseHours $ stripQuotes $ head [wh | [_, _, wh, _] <- parsedRows]
            shiftLengths = parseHours $ stripQuotes $ head [sl | [_, _, _, sl] <- parsedRows]
        in return EmployerRequirements {
            workHours = workHours,
            shiftLengths = shiftLengths,
            criticalMinimums = criticalMinimums
        }
    else
        error "Invalid row format in employer requirements file."


-- Inserts a key-value pair into a list of key-value pairs in order.
--
-- This function ensures that the list of key-value pairs is ordered by the keys, and it inserts the new key-value
-- pair in its correct position while maintaining the order.
--
-- Arguments:
--   key: The key to insert.
--   value: The value associated with the key.
--   list: The list of existing key-value pairs.
--
-- Returns:
--   The updated list of key-value pairs with the new pair inserted in order.
insertInOrder :: Eq k => k -> v -> [(k, v)] -> [(k, v)]
insertInOrder key value [] = [(key, value)]
insertInOrder key value ((k, v) : xs)
    | key == k  = (key, value) : xs
    | otherwise = (k, v) : insertInOrder key value xs


-- Parses a string representing hours into a tuple of integers.
--
-- This function expects a string in the format "start-end", where start and end are integers representing the start
-- and end times. It converts this string into a tuple of integers (start, end).
--
-- Arguments:
--   s: A string representing the hours in the format "start-end".
--
-- Returns:
--   A tuple of integers representing the start and end times.
parseHours :: String -> (Int, Int)
parseHours s = 
    case map read (splitDash s) of
        [start, end] -> (start, end)
        _ -> error $ "Invalid hour format: " ++ s



-- Utility Functions
splitCSV :: String -> [String]
splitCSV = splitCSVHelper False [] []

splitCSVHelper :: Bool -> String -> [String] -> String -> [String]
splitCSVHelper _ current acc [] = acc ++ [reverse current]
splitCSVHelper inQuotes current acc (x:xs)
    | x == '"' = splitCSVHelper (not inQuotes) current acc xs
    | x == ',' && not inQuotes = splitCSVHelper False [] (acc ++ [reverse current]) xs
    | otherwise = splitCSVHelper inQuotes (x:current) acc xs

splitDash :: String -> [String]
splitDash = splitBy '-'

splitSemi :: String -> [String]
splitSemi = splitBy ';'

splitBy :: Char -> String -> [String]
splitBy _ [] = []
splitBy delim s = let (first, rest) = span (/= delim) s
                  in first : case rest of
                      [] -> []
                      (_:xs) -> splitBy delim xs

stripQuotes :: String -> String
stripQuotes = filter (`notElem` ['"'])

splitComma :: String -> [String]
splitComma = splitBy ','
