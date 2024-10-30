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
   | backtrackLoop board board (length (head board)) (0,0) [] 0 == 1    = UniqueSolution
   | backtrackLoop board board (length (head board)) (0,0) [] 0 > 1     = MultipleSolutions
   | otherwise                                       = NoSolution

{-
instance Show Solutions where
    show NoSolution        = "NoSolution"
    show UniqueSolution    = "UniqueSolution"
    show MultipleSolutions = "MultipleSolutions"

uniqueBoard :: Board
uniqueBoard = [[0,10,2,0,0,8,9,3,11,4,0,7,1,5,12,0],
    [7,4,0,16,10,14,0,2,1,6,12,0,0,0,0,13],
    [0,6,1,12,0,7,0,0,3,13,9,8,0,0,15,0],
    [8,13,3,9,6,5,12,1,2,10,15,14,11,7,0,4],
    [9,0,0,5,11,12,0,6,10,3,8,15,4,16,0,2],
    [0,0,4,14,3,15,8,0,6,0,7,12,13,9,0,1],
    [12,11,0,7,2,16,14,0,13,0,5,9,0,0,8,3],
    [0,0,0,8,1,9,5,13,4,0,14,0,6,12,7,0],
    [0,0,9,6,0,11,4,0,15,8,0,3,16,2,10,0],
    [0,0,0,13,5,0,6,9,16,14,10,2,0,11,0,7],
    [11,7,12,4,14,0,0,16,9,5,6,0,15,3,13,0],
    [2,14,0,10,0,0,13,15,12,0,4,0,9,1,6,5],
    [4,16,7,2,15,0,0,0,5,0,0,6,0,13,0,9],
    [0,12,5,11,16,0,0,0,8,0,0,13,14,10,0,15],
    [13,9,8,1,12,6,11,5,14,15,0,0,7,4,2,16],
    [10,15,0,3,0,13,1,8,0,16,2,0,5,6,0,12]]
ambiguousBoard :: Board
ambiguousBoard = [[0,10,2,0,0,8,9,3,11,4,0,7,1,5,12,0],
    [7,4,0,16,10,14,0,2,1,6,12,0,0,0,0,13],
    [0,6,1,12,0,7,0,0,3,13,9,8,0,0,15,0],
    [8,13,3,9,6,5,12,1,2,10,15,14,11,7,0,4],
    [9,0,0,5,11,12,0,6,10,3,8,15,4,16,0,2],
    [0,0,4,14,3,15,8,0,6,0,7,12,13,9,0,1],
    [12,11,0,7,2,16,14,0,13,0,5,9,0,0,8,3],
    [0,0,0,8,1,9,5,13,4,0,14,0,6,12,7,0],
    [0,0,9,6,0,11,4,0,15,8,0,3,16,2,10,0],
    [0,0,0,13,5,0,6,9,16,14,10,0,0,11,0,7],
    [11,7,12,4,14,0,0,16,9,5,6,0,15,3,13,0],
    [2,14,0,10,0,0,13,15,12,0,4,0,9,1,6,5],
    [4,16,7,2,15,0,0,0,5,0,0,6,0,13,0,9],
    [0,12,5,11,16,0,0,0,8,0,0,13,14,10,0,15],
    [13,9,8,1,12,6,11,5,14,15,0,0,7,4,2,16],
    [10,15,0,3,0,13,1,8,0,16,2,0,5,6,0,12]]
invalidBoard :: Board
invalidBoard = [[0,10,2,0,0,8,9,3,11,4,0,7,1,5,12,0],
    [7,4,0,16,10,14,0,2,1,6,12,0,0,0,0,13],
    [0,6,1,12,0,7,0,0,3,13,9,8,0,0,15,0],
    [8,13,3,9,6,5,12,1,2,10,15,14,11,7,0,4],
    [9,0,0,5,11,12,0,6,10,3,8,15,4,16,0,2],
    [0,0,4,14,3,15,8,0,6,0,7,12,13,9,0,1],
    [12,11,0,7,2,16,14,0,13,0,5,9,0,0,8,3],
    [0,0,0,8,1,9,5,13,4,0,14,0,6,12,7,0],
    [0,0,9,6,0,11,4,0,15,8,0,3,16,2,10,0],
    [0,0,0,13,5,0,6,9,16,14,10,0,0,11,0,7],
    [11,7,12,4,14,0,0,16,9,5,6,0,15,3,13,0],
    [2,14,0,10,0,0,13,15,12,0,4,0,9,1,6,5],
    [4,16,7,2,15,0,0,0,5,0,0,6,0,13,0,9],
    [0,12,5,11,16,0,13,0,8,0,0,13,14,10,0,15],
    [13,9,8,1,12,6,11,5,14,15,0,0,7,4,2,16],
    [10,15,0,3,0,13,1,8,0,16,2,0,5,6,0,12]]
-}


--increase value of a cell by 1
increaseValue:: Board -> Int -> (Int, Int) -> Board
increaseValue oldBoard lengthBoard (row,col) =
    [if x == row then 
        [if y == col then ((oldBoard !! x !! y) + 1) `mod` (lengthBoard + 1) --if it goes above grid size, resets to 0 (all options exhausted)
        else oldBoard !! x !! y | y <- [0..lengthBoard - 1]]
     else oldBoard !! x | x <- [0..lengthBoard - 1]]

--Check if there are duplicates or number above size limit in a row
evaluateCombination:: [Int] -> Bool
evaluateCombination [] = True
evaluateCombination row =
  let size = length row
      cleanedRow = [x | x <- row, x /= 0]
  in length [x | x <- [1..size], x `elem` cleanedRow] == length cleanedRow



--obtains the subgrid of a given square, turns it into a list and evaluates it
evaluateSubgrid:: Board -> Int -> (Int,Int) -> Bool
evaluateSubgrid newBoard subgridLength (row, col) =
    let rowStart = row - (row `mod` subgridLength) --beginning of the subgrid's row in relation to the whole board
        colStart = col - (col `mod` subgridLength) --beginning of the subgrid's column in relation to the whole board
        rowEnd = rowStart + subgridLength - 1 --end of the subgrid's row in relation to the whole board, -1 to consider that lists start at 0
        colEnd = colStart + subgridLength - 1 --end of the subgrid's column in relation to the whole board, -1 to consider that lists start at 0
        evaluateList = [newBoard !! x !! y | x <- [rowStart..rowEnd], y <- [colStart..colEnd]]
    in evaluateCombination evaluateList


backtrackLoop :: Board -> Board -> Int -> (Int,Int) -> [(Int,Int)] -> Int -> Int
backtrackLoop oldBoard board lengthBoard (row,col) previousCoord count
    | row >= lengthBoard  = if null previousCoord then count --all routes exhausted
                            else if (evaluateCombination (board !! (lengthBoard -1)) &&
                                    evaluateCombination [board !! x !! (lengthBoard -1) | x <- [0..(lengthBoard -1)]] &&
                                    evaluateSubgrid board (round(sqrt (fromIntegral lengthBoard))) ((lengthBoard -1), (lengthBoard -1)))
                                    then backtrackLoop oldBoard board lengthBoard (head previousCoord) previousCoord (count + 1) --explored one possibility fully, is valid
                            else backtrackLoop oldBoard board lengthBoard (head previousCoord) previousCoord count --explored one possibility fully, is not valid

    | col >= lengthBoard = backtrackLoop oldBoard board lengthBoard (row + 1, 0) previousCoord count --reached end of a row, moving to the next
    | oldBoard !! row !! col /= 0 = backtrackLoop oldBoard board lengthBoard (row, col + 1) previousCoord count --square was not originally empty and can be skipped

    | otherwise =
        let updatePreviousCoord = if (board !! row !! col == 0) then (row,col):previousCoord else previousCoord --first time checking an empty square
            newBoard = increaseValue board lengthBoard (row,col)   --increment value +1
            subgridLength = round(sqrt (fromIntegral lengthBoard)) --assuming that board complies with n^2, it will never give a decimal result
            tpc = tail previousCoord
        in 
            if newBoard !! row !! col == 0 then --exhausted all numbers in that square in current branch
                if length previousCoord == 1 then count --no branches left
                else backtrackLoop oldBoard newBoard lengthBoard (head tpc) (tail previousCoord) count
            else if (evaluateCombination (newBoard !! row) &&
                    evaluateCombination [newBoard !! x !! col | x <- [0..(lengthBoard -1)]] &&
                    evaluateSubgrid newBoard subgridLength (row, col)) --legal combination
                    then backtrackLoop oldBoard newBoard lengthBoard (row, col + 1) updatePreviousCoord count --move on

                    else backtrackLoop oldBoard newBoard lengthBoard (row, col) updatePreviousCoord count -- illegal combination below length, try next number