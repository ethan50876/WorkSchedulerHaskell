-- InputParser.hs
module InputParser (
    parseEmployees,
    parseRequirements
) where

import System.IO
import qualified Data.Set as Set
import qualified Data.Map as Map
import DataManager

parseEmployees :: FilePath -> IO [Employee]
parseEmployees filePath = do
    content <- readFile filePath
    let ls = drop 1 (lines content) -- drop header
    return $ map parseEmployeeLine ls

parseEmployeeLine :: String -> Employee
parseEmployeeLine line =
    let parts = splitCSV line
        name = parts !! 0
        hoursAvail = parts !! 1
        minH = read (parts !! 2)
        maxH = read (parts !! 3)
        rolesStr = parts !! 4
        daysOffStr = if length parts > 5 then parts !! 5 else ""
        [startH, endH] = map read $ splitDash hoursAvail
        roles = Set.fromList $ map stripQuotes $ splitSemi rolesStr
        daysOff = Set.fromList $ map stripQuotes $ if null daysOffStr then [] else splitSemi daysOffStr
    in Employee name (startH, endH) minH maxH roles daysOff

splitCSV :: String -> [String]
splitCSV = splitBy ','

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
stripQuotes = filter (`notElem` ['\"'])

parseRequirements :: FilePath -> IO EmployerRequirements
parseRequirements filePath = do
    content <- readFile filePath
    let ls = drop 1 (lines content)
    let rows = map parseRequirementLine ls
    let workH = head [wh | (_, _, wh, _) <- rows]
    let shiftL = head [sl | (_, _, _, sl) <- rows]
    let minRoles = Map.fromList [(r, m) | (r, m, _, _) <- rows]
    return $ EmployerRequirements workH shiftL minRoles

parseRequirementLine :: String -> (String, Int, (Int, Int), (Int, Int))
parseRequirementLine line =
    let parts = splitCSV line
        role = parts !! 0
        minCount = read (parts !! 1)
        workH = let [a, b] = splitDash (parts !! 2) in (read a, read b)
        shiftL = let [c, d] = splitDash (parts !! 3) in (read c, read d)
    in (role, minCount, workH, shiftL)
