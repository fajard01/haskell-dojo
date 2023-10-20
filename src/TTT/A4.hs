module TTT.A4 where

import Data.List (transpose)
import TTT.A1
import TTT.A2
import TTT.A3 (getAllLines, putSquare)
import Data.List.NonEmpty (append)

_X_WIN_ = [ [X, O, O]
          , [O, X, O]
          , [O, O, X]
          ]

_O_WIN_ = [ [O, X, O]
          , [X, X, O]
          , [X, O, O]
          ]

-- Q#01
_HEADER_ :: String
_HEADER_ = " " ++ formatLine (map show _RANGE_)

-- Q#02
showSquares :: [Square] -> [String]
showSquares = map showSquare

-- Q#03
dropFirstCol :: Board -> Board
dropFirstCol = map tail

-- Q#04
dropLastCol :: Board -> Board
dropLastCol = map init

--Q#05
formatRows :: [Row] -> [String]
formatRows = map (\row -> formatLine (showSquares row))

-- Q#06
isWinningLine_ :: Player -> Line -> Bool
isWinningLine_ _      []   = False
isWinningLine_ player line = null (filter (/=player) line)

-- Q#07
isWinningLine :: Player -> Line -> Bool
isWinningLine _      []   = False
isWinningLine player line = foldr (\square bool -> (square == player) && bool) True line

-- Q#08
hasWon :: Player -> Board -> Bool
hasWon _      []    = False
hasWon player board = foldr (\line bool -> isWinningLine player line || bool) False (getAllLines board)

-- Q#09
getGameState :: Board -> GameState
getGameState board | hasWon X board           = X_Wins
                   | hasWon O board           = O_Wins
                   | E `notElem` concat board = Draw
                   | otherwise                = In_Progress

playMove :: Player -> Board -> Move -> (GameState, Board)
playMove _      []     _   = (Draw, [])
playMove player board move = (newState, updatedBoard)
                                where
                                    updatedBoard :: Board
                                    updatedBoard = putSquare player board move
                                    newState :: GameState
                                    newState = getGameState updatedBoard

-- Q#10
prependRowIndices :: [String] -> [String]
prependRowIndices strings = zipWith (++) indices strings
                                where
                                    indices :: [String]
                                    indices = map (: []) ['A'..]

-- Q#11
formatBoard :: Board -> String
formatBoard board = unlines . (:) _HEADER_ $ prependRowIndices $ formatRows board