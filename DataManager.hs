-- DataManager.hs
module DataManager (
    parseEmployeeData,
    parseEmployerRequirements
) where

import System.IO
import qualified Data.Set as Set
import qualified Data.Map as Map
import Employee
import EmployerRequirements

-- Parses Employee Data
parseEmployeeData :: FilePath -> IO [Employee]
parseEmployeeData filePath = do
    content <- readFile filePath
    let rows = tail $ lines content
    return $ map parseEmployee rows

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

-- Parses Employer Requirements Data
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

insertInOrder :: Eq k => k -> v -> [(k, v)] -> [(k, v)]
insertInOrder key value [] = [(key, value)]
insertInOrder key value ((k, v) : xs)
    | key == k  = (key, value) : xs
    | otherwise = (k, v) : insertInOrder key value xs

-- Parses Hours from string like "9-18"
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
