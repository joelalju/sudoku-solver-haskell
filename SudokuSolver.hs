-- \/\/\/ DO NOT MODIFY THE FOLLOWING LINES \/\/\/
module SudokuSolver(Board, Solutions(..), author, nickname, numSolutions) where
import Sudoku(Board, Solutions(..))

-- /\/\/\ DO NOT MODIFY THE PRECEDING LINES /\/\/\

{- (Remember to provide a brief (about 100-500 words) description of
   your implementation.)
 -}

author :: String
author = "Joel Alarcón Julian"  -- replace `undefined' with your first and last name

nickname :: String
nickname = "JohnGranblue"  -- replace `undefined' with a nickname for your solver

{- (Remember to provide a complete function specification.)
 -}
numSolutions :: Board -> Solutions
numSolutions [] = NoSolution
numSolutions board 
   | backtrackLoop board (length (head board)) (0,0) 0 == 1    = UniqueSolution
   | backtrackLoop board (length (head board)) (0,0) 0 > 1     = MultipleSolutions
   | otherwise                                       = NoSolution



-- TEST MAIN
main :: IO ()
main = do
    let result = backtrackLoop exampleBoard (length (head exampleBoard)) (0,0) 0
    let result2 = backtrackLoop resolvedSudoku (length (head resolvedSudoku)) (0,0) 0
    print result
    print result2



-- Sample 9x9 Sudoku board (0 represents empty cells)
exampleBoard :: Board
exampleBoard =
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

resolvedSudoku :: Board
resolvedSudoku =
    [ [5, 3, 4, 6, 7, 8, 9, 1, 2]
    , [6, 7, 2, 1, 9, 5, 3, 4, 8]
    , [1, 9, 8, 3, 4, 2, 5, 6, 7]
    , [8, 5, 9, 7, 6, 1, 4, 2, 3]
    , [4, 2, 6, 8, 5, 3, 7, 9, 1]
    , [7, 1, 3, 9, 2, 4, 8, 5, 6]
    , [9, 6, 1, 5, 3, 7, 2, 8, 4]
    , [2, 8, 7, 4, 1, 9, 6, 3, 5]
    , [3, 4, 5, 2, 8, 6, 1, 7, 9]
    ]



--Board, row index, lengthBoard, true/false
evaluateCombination:: [Int] -> Int
evaluateCombination [] = True
evaluateCombination row do
  let size = length row
  [x | x <- [0..size], x `elem` row]
  

--counts the amount of 0s in the sudoku board
backtrackLoop :: Board -> Int -> (Int,Int) -> Int -> Int
backtrackLoop board lengthBoard (row,col) count
    | row >= lengthBoard = count -- reached end of board
    | col >= lengthBoard = backtrackLoop board lengthBoard (row + 1, 0) count
    | board !! row !! col == 0 = backtrackLoop board lengthBoard (row, col + 1) (count + 1)
    | otherwise = backtrackLoop board lengthBoard (row, col + 1) count




--counts the amount of 0s in the sudoku board
countLoop :: Board -> Int -> (Int,Int) -> Int -> Int
backtrackLoop board lengthBoard (row,col) count
    | row >= lengthBoard = count -- reached end of board
    | col >= lengthBoard = backtrackLoop board lengthBoard (row + 1, 0) count
    | board !! row !! col == 0 = backtrackLoop board lengthBoard (row, col + 1) (count + 1)
    | otherwise = backtrackLoop board lengthBoard (row, col + 1) count