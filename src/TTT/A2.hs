module TTT.A2 where

import Data.List (intercalate)
import TTT.A1

-- Q#01
promptPlayer :: Player -> String
promptPlayer player = concat ["Player ", show player, "'s turn: enter a row and column position (ex. A1)"]

-- Q#02
_RANGE_ :: [Int]
_RANGE_ = [0..(_SIZE_ - 1)]

-- Q#03
isDigit :: Char -> Bool
isDigit char = char `elem` "0123456789"

readDigit :: Char -> Int
readDigit char = if isDigit char
                       then read [char]
                       else -1

-- Q#04
_EMPTY_ROW_ :: Row
_EMPTY_ROW_ = replicate _SIZE_ E

_EMPTY_BOARD_ :: [Row]
_EMPTY_BOARD_ = replicate _SIZE_ _EMPTY_ROW_

-- Q#05
isTied :: Board -> Bool
isTied board = if E `elem` concat board
                   then False
                   else True

_TIED_BOARD_ :: Board
_TIED_BOARD_ = [ [X,O,O]
               , [O,X,X]
               , [O,X,O]
               ]

-- Q#06
indexRowStrings :: [String] -> [(Char,String)]
indexRowStrings xs = zip ['A'.. ] xs

-- Q#07
formatLine :: [String] -> String
formatLine xss = _SEP_ ++ intercalate _SEP_ xss ++ _SEP_

-- Q#08
isMoveInBounds :: Move -> Bool
isMoveInBounds move = (fst move >= 0 && fst move < _SIZE_) &&
                      (snd move >= 0 && snd move < _SIZE_)

-- Q#09
stringToMove :: String -> Move
stringToMove []       = _INVALID_MOVE_
stringToMove (x:y:xs) | xs /= []      = _INVALID_MOVE_
                      | otherwise     = (toInt x, toInt y)
                            where
                                toInt :: Char -> Int
                                toInt char = if isDigit char
                                                then readDigit char
                                                else convertRowIndex char

-- Q#10
replaceSquareInRow :: Player -> Int -> Row -> Row
replaceSquareInRow player index row | index < 0 || index > length row-1 = row
                                    | otherwise                         = xs ++ [player] ++ ys
                                        where
                                            (xs,y:ys) = splitAt index row

rsX :: Int -> Row -> Row
rsX index row = replaceSquareInRow X index row

rsO :: Int -> Row -> Row
rsO index row = replaceSquareInRow O index row