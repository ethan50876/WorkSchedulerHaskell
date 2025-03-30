-- Employee.hs
module Employee (
    Employee(..),
    createRoleSet,
    createDaysOffSet
) where

import Data.Set as Set

-- Employee Data Type
data Employee = Employee {
    name :: String,
    availability :: (Int, Int),
    minHours :: Int,
    maxHours :: Int,
    roles :: Set.Set String,
    daysOff :: Set.Set String
} deriving (Show, Eq)

-- Critical Role Constructor
createRoleSet :: [String] -> Set.Set String
createRoleSet = Prelude.foldl (flip Set.insert) Set.empty


-- DaysOff Constructor
createDaysOffSet :: [String] -> Set.Set String
createDaysOffSet = Prelude.foldl (flip Set.insert) Set.empty
