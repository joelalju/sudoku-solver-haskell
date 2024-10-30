-- \/\/\/ DO NOT MODIFY THE FOLLOWING LINES \/\/\/
module SudokuSolver(Board, Solutions(..), author, nickname, numSolutions) where
import Sudoku(Board, Solutions(..))

-- /\/\/\ DO NOT MODIFY THE PRECEDING LINES /\/\/\

import Debug.Trace (traceShow)

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



-- TEST MAIN
main :: IO ()
main = do
    let result = backtrackLoop uniqueBoard1 uniqueBoard1 (length (head uniqueBoard1)) (0,0) [] 0
    let result2 = backtrackLoop ambiguousBoard1 ambiguousBoard1 (length (head ambiguousBoard1)) (0,0) [] 0
    let result3 = backtrackLoop invalidBoard1 invalidBoard1 (length (head invalidBoard1)) (0,0) [] 0
    print result
    print result2
    print result3
    

uniqueBoard1 :: Board
uniqueBoard1 = [[0,1,6,3,9,2,0,5,0],
    [4,5,8,6,0,7,0,9,3],
    [2,0,3,0,0,4,7,1,0],
    [0,0,0,0,3,5,0,0,0],
    [1,0,0,7,6,9,5,3,2],
    [0,3,2,4,0,0,9,0,0],
    [8,0,0,1,4,0,3,7,0],
    [3,0,9,5,2,8,0,4,0],
    [6,0,0,9,0,0,0,0,5]]
ambiguousBoard1::Board
ambiguousBoard1 = [[5,0,2,6,9,0,4,3,7],
    [0,9,0,0,7,0,0,5,1],
    [0,7,0,5,0,0,0,0,9],
    [2,3,0,8,5,9,0,4,6],
    [0,5,0,4,6,7,1,2,3],
    [0,6,7,0,3,1,9,8,5],
    [0,0,0,0,4,3,5,9,0],
    [0,4,3,0,2,0,6,7,8],
    [9,2,5,0,0,6,3,0,0]]
invalidBoard1::Board
invalidBoard1 = [[0,1,6,3,9,2,0,5,0],
    [4,5,8,6,0,7,0,9,3],
    [2,0,3,0,0,4,7,1,0],
    [0,0,0,0,3,5,0,0,0],
    [1,0,0,7,0,9,5,3,2],
    [0,3,2,4,0,0,9,0,0],
    [8,0,0,1,4,0,3,7,0],
    [3,0,9,5,2,8,7,4,0],
    [6,0,0,9,0,0,0,0,5]]

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
                                    then backtrackLoop oldBoard board lengthBoard (head previousCoord) (tail previousCoord) (count + 1)--explored one possibility fully
                            else backtrackLoop oldBoard board lengthBoard (head previousCoord) (tail previousCoord) count--explored one possibility fully

    | col >= lengthBoard = backtrackLoop oldBoard board lengthBoard (row + 1, 0) previousCoord count --reached end of a row, moving to the next
    | oldBoard !! row !! col /= 0 = backtrackLoop oldBoard board lengthBoard (row, col + 1) previousCoord count --square was not originally empty and can be skipped

    | otherwise =
        let updatePreviousCoord = if (board !! row !! col == 0) then (row,col):previousCoord else previousCoord --first time checking an empty square
            newBoard = increaseValue board lengthBoard (row,col)   --increment value +1
            subgridLength = round(sqrt (fromIntegral lengthBoard)) --assuming that board complies with n^2, it will never give a decimal result
            tpc = tail previousCoord

        in --traceShow (previousCoord, count) $

            if newBoard !! row !! col == 0 then --exhausted all numbers in that square in current branch
                if length previousCoord == 1 then count --no branches left
                else backtrackLoop oldBoard newBoard lengthBoard (head tpc) (tail previousCoord) count
            else if (evaluateCombination (newBoard !! row) &&
                    evaluateCombination [newBoard !! x !! col | x <- [0..(lengthBoard -1)]] &&
                    evaluateSubgrid newBoard subgridLength (row, col)) --legal combination

                    then backtrackLoop oldBoard newBoard lengthBoard (row, col + 1) updatePreviousCoord count
                    else backtrackLoop oldBoard newBoard lengthBoard (row, col) updatePreviousCoord count -- illegal combination below length, try next number





 --   | board !! row !! col  < lengthBoard = 
 --       let newBoard = increaseValue board lengthBoard (row,col)
 --           subgridLength = round(sqrt (fromIntegral lengthBoard)) --assuming that board complies with n^2, it will never give a decimal result
 --       in  traceShow newBoard $
 --           if (evaluateCombination (newBoard !! row) &&
 --               evaluateCombination (newBoard !! col) &&
 --               evaluateSubgrid newBoard subgridLength (row, col)
 --               )
 --               then backtrackLoop oldBoard newBoard lengthBoard (row, col + 1) count --new iteration with the updated board
 --           else if newBoard !! row !! col == 0
 --               then 
 --                 if col /= 0 then backtrackLoop oldBoard board lengthBoard (row, col - 1) count -- if we're not at the first column, backtrack column
 --                 else if row /= 0 then backtrackLoop oldBoard board lengthBoard (row - 1, col) count -- if we're not at the first row, backtrack row
 --                 else count -- if col and row = 0, then we've exhausted all possibilities
 --               else backtrackLoop oldBoard newBoard lengthBoard (row, col) count
--    | otherwise = backtrackLoop oldBoard board lengthBoard (row, col + 1) count -- current square has 9, move to the next


--counts the amount of 0s in the sudoku board
countLoop :: Board -> Int -> (Int,Int) -> Int -> Int
countLoop board lengthBoard (row,col) count
    | row >= lengthBoard = count -- reached end of board
    | col >= lengthBoard = countLoop board lengthBoard (row + 1, 0) count
    | board !! row !! col == 0 = countLoop board lengthBoard (row, col + 1) (count + 1)
    | otherwise = countLoop board lengthBoard (row, col + 1) count