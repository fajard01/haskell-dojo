module TTT.A3 where

import Data.List
import TTT.A1
import TTT.A2

-- Q#01
showInts :: [Int] -> [String]
showInts []     = []
showInts (x:xs) = show x : showInts xs

_HEADER_ :: String
_HEADER_ = " " ++ formatLine (showInts _RANGE_)

-- Q#02
showSquares :: [Square] -> [String]
showSquares []     = []
showSquares (square:squares) = showSquare square : showSquares squares

-- Q#03
formatRows :: [Row] -> [String]
formatRows []          = []
formatRows (row:rows) = formatLine (showSquares row) : formatRows rows

-- Q#04
isColEmpty :: Row -> Int -> Bool
isColEmpty row index | null row                         = False
                     | index < 0 || index >= length row = False
                     | otherwise                        = if row !! index == E
                                                                then True
                                                                else False
-- Q#05
dropFirstCol :: Board -> Board
dropFirstCol []         = []
dropFirstCol (row:rows) = tail row : dropFirstCol rows

dropLastCol :: Board -> Board
dropLastCol []         = []
dropLastCol (row:rows) = init row : dropLastCol rows

-- Q#06
getDiag1 :: Board -> Line
getDiag1 []         = []
getDiag1 (row:rows) = head row : getDiag1 (dropFirstCol rows)

getDiag2 :: Board -> Line
getDiag2 []         = []
getDiag2 (row:rows) = last row : getDiag2 (dropLastCol rows)

getAllLines :: Board -> [Line]
getAllLines board = concat [board, transpose board, [getDiag1 board], [getDiag2 board]]

-- Q#07
putSquare :: Player -> Board -> Move -> Board
putSquare _      []         _     = []
putSquare player (row:rows) (x,y) | x == 0    = replaceSquareInRow player y row : rows
                                  | otherwise = row : putSquare player rows (x-1,y)

-- Q#08
prependRowIndices :: [String] -> [String]
prependRowIndices []      = []
prependRowIndices strings = prependIndex indexRows
                                where
                                    indexRows :: [(Char,String)]
                                    indexRows = indexRowStrings strings
                                    prependIndex :: [(Char,String)] -> [String]
                                    prependIndex []               = []
                                    prependIndex ((index,row):xs) = (index:row) : prependIndex xs

-- Q#09
isWinningLine_ :: Player -> Line -> Bool
isWinningLine_ player []   = False
isWinningLine_ player line = lineWin player line
                                where
                                    lineWin :: Player -> Line -> Bool
                                    lineWin player [] = True
                                    lineWin player (square:squares) | square /= player = False
                                                                    | otherwise        = lineWin player squares

-- Q#10
isValidMove :: Board -> Move -> Bool
isValidMove []         _     = False
isValidMove (row:rows) (x,y) | not (isMoveInBounds (x,y)) = False
                             | x == 0                     = isColEmpty row y
                             | otherwise                  = isValidMove rows (x-1,y)