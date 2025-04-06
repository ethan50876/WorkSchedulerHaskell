-- ShiftSchedule.hs
--
-- This module defines the data type 'ShiftSchedule', which is used to represent the entire shift schedule of employees
-- across multiple days and roles. It provides a structure to map days of the week to hourly shifts, which in turn are
-- mapped to roles and assigned employees.
--
-- The 'ShiftSchedule' type is a nested map structure, which allows for direct representation of employee assignments 
-- by day, hour, and role.
--
module ShiftSchedule (
    ShiftSchedule
) where

import Employee (Employee)
import qualified Data.Map as Map

-- ShiftSchedule type alias.
--
-- Represents the shift schedule for employees, organized by day, hour, and role.
-- It is a nested map structure:
-- 
-- * The outer 'Map.Map String' represents the days of the week ("Monday", "Tuesday").
-- * The second 'Map.Map Int' represents the hours of the day.
-- * The innermost 'Map.Map String [Employee]' represents roles ("Cashier", "Supervisor") and the list of employees assigned to each role at that hour.
--
type ShiftSchedule = Map.Map String (Map.Map Int (Map.Map String [Employee]))