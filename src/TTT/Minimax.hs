module TTT.Minimax where

{-
This is an extra module outside of the regular haskell-dojo course.
This implements a version of tictactoe to play against the  computer by using the minimax algorithm.
-}

import TTT.A1
import TTT.A2
import TTT.A5
import TTT.A4 (playMove)

-- A tree structure for the game tree
data Tree a = Node a [Tree a]
                deriving Show

gameTree :: Player -> Board -> Tree Board
gameTree board player =
    Node board [gameTree board' (switchPlayer player) | board' <- getAllValidMoves player board]

getAllValidMoves :: Player -> Board -> [Board]
getAllValidMoves player board =
    case getGameState board of
        X_Wins      -> []
        O_Wins      -> []
        Draw        -> []
        In_Progress -> let rows        = [readDigit c | c <- (take _SIZE_ ["A" ..]]
                           cols        = [0 .. _SIZE_ - 1]
                           moves       = [x ++ y | x <- rows, y <- cols]
                           stateboards = [playMove player board move | move <- moves, isValidMove move]
                       in [board' | (_,board') <- stateboards]