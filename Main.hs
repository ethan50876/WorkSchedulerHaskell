module Main where

import InputParser
import OutputWriter
import Employee
import EmployerRequirements

main :: IO ()
main = do
    let employeePath = "data/employees.csv"
    let requirementsPath = "data/requirements.csv"

    employees <- parseEmployees employeePath
    requirements <- parseRequirements requirementsPath

    echoEmployees employees
    echoRequirements requirements
