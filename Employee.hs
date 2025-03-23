module Employee (Employee(..)) where
import Data.Set as Set
data Employee = Employee
  { name        :: String
  , availability :: (Int, Int)
  , minHours    :: Int
  , maxHours    :: Int
  , roles       :: Set String
  , daysOff     :: Set String
  } deriving (Show, Eq)
