# 📅 Work Shift Scheduler

COMP 3649 Final Project  
**Authors**: Ethan Ai, Kushal Saini

---

## Overview

The Work Shift Scheduler generates weekly employee schedules based on:
- Employee availability, roles, and weekly hour limits
- Employer role minimums, operating hours, and shift length constraints

Built using **Haskell**, applying **pure functional programming** and **recursive backtracking**.

---

## Project Structure

- `Main.hs` — Program entry and control flow
- `DataManager.hs` — CSV parsing and validation
- `SchedulerCore.hs` — Core scheduling and backtracking
- `SchedulerMinHrs.hs` — Fulfills employee minimum hours
- `SchedulerOps.hs` — Add/remove assignments
- `Validators.hs` — Assignment constraint checks
- `ShiftSchedule.hs`, `SchedulerContext.hs`, `SchedulerState.hs` — ADTs and environment

---

## Input Files

Place files in `data/` folder:

- `employees.csv`: Employee preferences
- `requirements.csv`: Employer role requirements

---

## How to Run

```bash
make          # Compile
./Scheduler.exe  # Run
make clean    # Clean build files
