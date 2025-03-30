-- ShiftSchedule.hs
module ShiftSchedule (
    ShiftSchedule
) where

import Employee (Employee)
import qualified Data.Map as Map

-- Shift Schedule Type Alias
type ShiftSchedule = Map.Map String (Map.Map Int (Map.Map String [Employee]))