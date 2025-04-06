-- Employee.hs
-- 
-- This module defines the 'Employee' data type and related functions to work with employee information,
-- including roles, working hours, and days off.
-- 
-- The 'Employee' data type includes fields for an employee's name, their working hours, the roles they
-- can perform, and their days off. This module also includes functions to create sets of roles and days
-- off from lists of strings.
--
-- The module exposes the following functions:
--
-- * 'createRoleSet': Creates a set of roles from a list of role names.
-- * 'createDaysOffSet': Creates a set of days off from a list of day names.
module Employee (
    Employee(..),
    createRoleSet,
    createDaysOffSet
) where

import Data.Set as Set

-- Employee Data Type
data Employee = Employee {
    name :: String,             -- Employee's name
    availability :: (Int, Int), -- The hours range (start, end)
    minHours :: Int,            -- Minimum working hours
    maxHours :: Int,            -- Maximum working hours
    roles :: Set.Set String,    -- Set of roles the employee can perform
    daysOff :: Set.Set String   -- Set of days off
} deriving (Show, Eq)

-- Critical Role Constructor
createRoleSet :: [String] -> Set.Set String
createRoleSet = Prelude.foldl (flip Set.insert) Set.empty


-- DaysOff Constructor
createDaysOffSet :: [String] -> Set.Set String
createDaysOffSet = Prelude.foldl (flip Set.insert) Set.empty
