module Sudoku where

import Data.List (nub)
import Data.Maybe (isJust)

type Board = [[Int]]

-- Check if the board is valid
isValid :: Board -> Bool
isValid board = all (== True) $ rowsValid ++ colsValid ++ boxesValid
  where
    rowsValid = map isUnitValid board
    colsValid = map isUnitValid (transpose board)
    boxesValid = map isUnitValid (boxes board)

isUnitValid :: [Int] -> Bool
isUnitValid unit = let filtered = filter (/= 0) unit in length filtered == length (nub filtered)

transpose :: Board -> Board
transpose ([]:_) = []
transpose board = map head board : transpose (map tail board)

boxes :: Board -> [[Int]]
boxes = concatMap (map concat . transpose . map (chunksOf 3)) . chunksOf 3

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)

-- Solve the Sudoku board
solve :: Board -> Maybe Board
solve board
  | not (isValid board) = Nothing
  | isSolved board = Just board
  | otherwise = listToMaybe . catMaybes $ map solve (nextBoards board)

isSolved :: Board -> Bool
isSolved = all (all (/= 0))

nextBoards :: Board -> [Board]
nextBoards board = [replace (x, y) v board | v <- [1..9]]
  where
    (x, y) = head [(i, j) | i <- [0..8], j <- [0..8], board !! i !! j == 0]

replace :: (Int, Int) -> Int -> Board -> Board
replace (x, y) v board = take x board ++ [take y (board !! x) ++ [v] ++ drop (y + 1) (board !! x)] ++ drop (x + 1) board

-- Sample initial board
initialBoard :: Board
initialBoard = 
  [ [5, 3, 0, 0, 7, 0, 0, 0, 0]
  , [6, 0, 0, 1, 9, 5, 0, 0, 0]
  , [0, 9, 8, 0, 0, 0, 0, 6, 0]
  , [8, 0, 0, 0, 6, 0, 0, 0, 3]
  , [4, 0, 0, 8, 0, 3, 0, 0, 1]
  , [7, 0, 0, 0, 2, 0, 0, 0, 6]
  , [0, 6, 0, 0, 0, 0, 2, 8, 0]
  , [0, 0, 0, 4, 1, 9, 0, 0, 5]
  , [0, 0, 0, 0, 8, 0, 0, 7, 9]
  ]
