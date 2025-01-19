-- \/\/\/ DO NOT MODIFY THE FOLLOWING LINES \/\/\/
module SudokuSolver(Board, Solutions(..), author, nickname, numSolutions) where
import Sudoku(Board, Solutions(..))

-- /\/\/\ DO NOT MODIFY THE PRECEDING LINES /\/\/\

{- (Remember to provide a brief (about 100-500 words) description of
   your implementation.)

    numSolutions provides how many solutions a sudoku board has by calling the recursive function backtrackLoop, which indicates how many total solutions said board has.
    backtrackLoop applies a brute force search backtracking method, as indicated by the wikipedia page https://en.wikipedia.org/wiki/Sudoku_solving_algorithms.
    The logic behind it is that, on each iteration of the function, I'm increasing the value of a given cell by, I check whether the combination of values for the row, column and subgrid related to the cell are valid (not counting empty cells) and then, depending whether or not all checks are passed, I move to the next cell or increase the value of the cell further. If all possibilities for a given cell are exhausted without a combination being valid, it means that branch of possibilities is not valid and I rollback to the previous modified value.
    The way I rollback to previous values is by doing a stack of coordinates called previousCoord, which I update every single time I modify a cell with a 0 value (which means I'm checking a cell from scratch) and I pop every time we need to go to the previous modified cell. If no values are left in previousCoord, it means that all possibilities have been exhausted.

    The function is divided in four steps: 
    First, I check whether the row "pointer" has gone beyond the size of the board. If so, it means that all rows have been updated and are valid by sudoku conventions if the last cell is valid. This means there is a complete board combination and I increase the counter (if valid) before going back to the last previousCoord.
    Second, I check whether the column "pointer" has gone beyond the size of the board. This simply means that I need to jump to the next row, so I jump to (row+1,0).
    Third, I check whether a cell I'm currently pointing at has a predefined value, which means I can't modify and has to be skipped. I achieve this through checking the original board given (oldBoard).
    Fourth, knowing that I am in a modifiable cell inside the scope of the grid, I proceed to increase its value and go through all the validations before moving on to the next cell or trying the next value. I also have the backtracking logic in this step if the value overflows back to 0 (exhausted all options).

    The value increase and validations have their own functions for ease of use. The validation works by checking whether all numbers in a given array (after deleting all 0s) are not duplicated and represented between 1 and the size of the array, which will be its square.
    I utilize said function evaluateCombination straight away when checking rows and columns, while I turn a subgrid into an array first through evaluateSubgrid before calling evaluateCombination to make it readable.
 -}

author :: String
author = "Joel Alarcón Julian"  -- replace `undefined' with your first and last name

nickname :: String
nickname = "JohnGranblue"  -- replace `undefined' with a nickname for your solver

{- (Remember to provide a complete function specification.)
 -}

{-  numSolutions board
    Indicates if a sudoku has none, one or more than one solution
    PRE: The length of the Board is the square of a positive integer, the board is square, the board contains only numbers between 0 and the length of the Board (inclusive), 0 indicates an empty cell.
    RETURNS: a data "Solutions"
    EXAMPLES: 
    numSolutions [[0,10,2,0,0,8,9,3,11,4,0,7,1,5,12,0],
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
    [10,15,0,3,0,13,1,8,0,16,2,0,5,6,0,12]] = UniqueSolution
-}
numSolutions :: Board -> Solutions
numSolutions [] = NoSolution
numSolutions board 
   | backtrackLoop board board (length (head board)) (0,0) [] 0 == 1    = UniqueSolution
   | backtrackLoop board board (length (head board)) (0,0) [] 0 > 1     = MultipleSolutions
   | otherwise                                       = NoSolution

{-  increaseValue oldBoard lengthBoard (row,col)
    Increases the value of a cell in a sudoku board by 1.
    PRE:  row,col are lower than lengthBoard and positive (including 0).
    RETURNS: oldBoard with the cell indicated by row,col increased by 1 (if the increased value were to increase above the value of lengthBoard, then it'll be 0)
    EXAMPLES:
    increaseValue [[0,10,2,0,0,8,9,3,11,4,0,7,1,5,12,0],
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
        [10,15,0,3,0,13,1,8,0,16,2,0,5,6,0,12]] 16 (0,0)  =

        [[1,10,2,0,0,8,9,3,11,4,0,7,1,5,12,0],
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
-}
increaseValue:: Board -> Int -> (Int, Int) -> Board
increaseValue oldBoard lengthBoard (row,col) =
    [if x == row then 
        [if y == col then ((oldBoard !! x !! y) + 1) `mod` (lengthBoard + 1) --if it goes above grid size, resets to 0 (all options exhausted)
        else oldBoard !! x !! y | y <- [0..lengthBoard - 1]]
     else oldBoard !! x | x <- [0..lengthBoard - 1]]

{-  evaluateCombination row
    Checks if there are duplicates or number above size limit in a given row
    RETURNS: A bool indicating whether or not there are two equal numbers (not counting 0) or numbers above the size of the dataset in a given array
    EXAMPLES: evaluateCombination [0,0,1,2,3] = False
    evaluateCombination [3,2,1] = True
-}
evaluateCombination:: [Int] -> Bool
evaluateCombination [] = True
evaluateCombination row =
  let size = length row
      cleanedRow = [x | x <- row, x /= 0]
  in length [x | x <- [1..size], x `elem` cleanedRow] == length cleanedRow


{-  evaluateSubgrid newBoard subgridLength (row, col) 
    Obtains the subgrid of a given square, turns it into a list and evaluates it
    PRE:  The length of the Board is the square of a positive integer, the board is square.
          row and col are lower than the size of an array in newBoard.
    RETURNS: A bool indicating whether or not the subgrid delimited by subgridLength and (row,col) contains no duplicate values (not counting 0) and said values 
            are not higher than the size of the grid.
    EXAMPLES: 
    evaluateSubgrid [[0,1,6,3,9,2,0,5,0],
    [4,5,8,6,0,7,0,9,3],
    [2,0,3,0,0,4,7,1,0],
    [0,0,0,0,3,5,0,0,0],
    [1,0,0,7,6,9,5,3,2],
    [0,3,2,4,0,0,9,0,0],
    [8,0,0,1,4,0,3,7,0],
    [3,0,9,5,2,8,0,4,0],
    [6,0,0,9,0,0,0,0,5]] 3 (0,2) = True
-}
evaluateSubgrid:: Board -> Int -> (Int,Int) -> Bool
evaluateSubgrid newBoard subgridLength (row, col) =
    let rowStart = row - (row `mod` subgridLength) --beginning of the subgrid's row in relation to the whole board
        colStart = col - (col `mod` subgridLength) --beginning of the subgrid's column in relation to the whole board
        rowEnd = rowStart + subgridLength - 1 --end of the subgrid's row in relation to the whole board, -1 to consider that lists start at 0
        colEnd = colStart + subgridLength - 1 --end of the subgrid's column in relation to the whole board, -1 to consider that lists start at 0
        evaluateList = [newBoard !! x !! y | x <- [rowStart..rowEnd], y <- [colStart..colEnd]]
    in evaluateCombination evaluateList


{-  backtrackLoop oldBoard board lengthBoard (row,col) previousCoord count
    Indicates whether a board contains 0, 1 or +1 solution following sudoku rules.
    PRE:  The length of oldBoard and board is the square of a positive integer, the boards are squares,
          the boards contain only numbers between 0 and the length of the Board (inclusive), 0 indicates an empty cell.
          board and oldBoard have the same grid size.
          (row,col) values cannot be above lengthBoard
          lengthBoard has to be equal to the size of either board
          all values before (row,col) are valid as per sudoku ruling.
    RETURNS: how many possible sudoku solutions oldBoard has.
    EXAMPLES:
    backtrackLoop [[0,1,6,3,9,2,0,5,0],
    [4,5,8,6,0,7,0,9,3],
    [2,0,3,0,0,4,7,1,0],
    [0,0,0,0,3,5,0,0,0],
    [1,0,0,7,6,9,5,3,2],
    [0,3,2,4,0,0,9,0,0],
    [8,0,0,1,4,0,3,7,0],
    [3,0,9,5,2,8,0,4,0],
    [6,0,0,9,0,0,0,0,5]] [[0,1,6,3,9,2,0,5,0],
    [4,5,8,6,0,7,0,9,3],
    [2,0,3,0,0,4,7,1,0],
    [0,0,0,0,3,5,0,0,0],
    [1,0,0,7,6,9,5,3,2],
    [0,3,2,4,0,0,9,0,0],
    [8,0,0,1,4,0,3,7,0],
    [3,0,9,5,2,8,0,4,0],
    [6,0,0,9,0,0,0,0,5]] 9 (0,0) [] 0 = 1
-}
-- VARIANT:  length (row,col), previousCoord
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