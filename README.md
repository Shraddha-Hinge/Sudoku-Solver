# Sudoku Solver Project

This Haskell project implements a Sudoku solver that efficiently solves any given Sudoku puzzle. The project demonstrates key functional programming concepts, including recursion, list operations, and immutability, within the Haskell language.

## Table of Contents

- [Introduction](#introduction)
- [Components](#components)
  - [Data Structures](#data-structures)
  - [Solver Algorithm](#solver-algorithm)
  - [User Interface](#user-interface)
- [Usage](#usage)

## Introduction

The Sudoku Solver is designed to take a partially filled Sudoku grid and solve it using a backtracking algorithm. It serves as a tool for understanding functional programming techniques and how they can be applied to solve constraint satisfaction problems in Haskell.

## Components

### Data Structures

- **Grid**: Represents the Sudoku grid as a list of lists (a 2D array) where each element is an integer (0 representing an empty cell).
- **Cell**: Represents a single cell in the grid, storing its position and value.

### Solver Algorithm

- **Backtracking**: The solver uses a recursive backtracking algorithm to attempt filling in each cell of the grid with a valid number. If a conflict is detected, the algorithm backtracks to try a different number.

### User Interface

- The user interface is command-line based, allowing users to input a Sudoku puzzle as a text-based grid, run the solver, and display the solved grid in the terminal.

## Usage

To use this project, follow these steps:

1. Ensure you have GHC (the Glasgow Haskell Compiler) installed on your system.
2. Clone or download this repository.
3. Navigate to the project directory in your terminal.
4. Compile the project using `ghc` with the `ghc --make sudokusolver.hs` command.
5. Run the executable `./sudokusolver` to start the application.
6. Input your Sudoku puzzle through the command-line interface.
7. The program will output the solved Sudoku grid if a solution exists.

