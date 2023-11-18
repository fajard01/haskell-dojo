module TTT.Minimax where

{-
The MiniMax Algorithm
This is an extra module outside of the regular haskell-dojo course.
This implements a version of tictactoe to play against the  computer by using the minimax algorithm.
-}

import TTT.A1
import TTT.A2 ( _EMPTY_BOARD_ )
import TTT.A3 ( isValidMove )
import TTT.A4 ( getGameState, playMove, showSquares )


-------------------- Generic Tree Data Structure and Functions --------------------


-- A tree structure for the board tree
-- Instead of having a separate data constructor for Leaf, Node a [] == Leaf
data Tree a = Node a [Tree a]
                deriving Show

type Depth = Int

-- Specify the depth of tree to be calculated to limit time and memory
_TREE_DEPTH_ :: Int
_TREE_DEPTH_ = 9

-- Prune the tree to specific depth
pruneTree :: Int -> Tree a -> Tree a
pruneTree 0 (Node root _)        = Node root []
pruneTree n (Node root branches) = Node root [pruneTree (n-1) branch | branch <- branches]

-- Calculate the depth of node from root to leaves inclusive of all branches
pathDepth :: Tree a -> Depth
pathDepth (Node root [])       = 1
pathDepth (Node root branches) = 1 + sum [pathDepth branch | branch <- branches]


------------------------ TicTacToe Minimax Algorithm Implementation ----------------- 


-- A board tree derived from all allowable board configurations from player's starting node
boardTree :: Player -> Board -> Tree Board
boardTree player board =
    Node board [boardTree (switchPlayer player) board' | board' <- getAllValidBoards player board]


-- All possible moves of size _SIZE_
-- Could've been done purely with ints but implemented instead with the spirit of the board implementation
getAllMoves :: [Move]
getAllMoves = [(x,y) | x <- rows, y <- cols]
                where
                    rows :: [Int]
                    rows = [0 .. _SIZE_ - 1]
                    cols :: [Int]
                    cols = [convertRowIndex c | c <- take _SIZE_ ['a' ..]]

-- Generate all possible board configurations by applying all allowed moves by player to current board                    
getAllValidBoards :: Player -> Board -> [Board]
getAllValidBoards player board =
    case getGameState board of
        X_Wins      -> []
        O_Wins      -> []
        Is_Draw     -> []
        In_Progress -> [board' | (_,board') <- stateboards]
            where
                stateboards :: [(GameState, Board)]
                stateboards = [playMove player board move | move <- getAllMoves, isValidMove board move]

-- Check which players turn it is next by counting the number of player's square in the board
-- Default first player is X (from getFirstPlayer True)
nextPlayer :: Board -> Player
nextPlayer board = if xs < os then X else O
                    where
                        xs :: Int
                        xs      = length $ filter (==X) squares 
                        os :: Int
                        os      = length $ filter (==O) squares
                        squares :: [Player]
                        squares = concat board

-- Minimax Algorithm
miniMax :: Tree Board -> Tree (Board, Player)
miniMax (Node board []) =
    case getGameState board of
        X_Wins      -> Node (board,X) []
        O_Wins      -> Node (board,O) []
        Is_Draw     -> Node (board,E) []
        In_Progress -> Node (board,E) []
miniMax (Node board trees) 
    | nextPlayer board == X = Node (board, minimum players) trees'
    | nextPlayer board == O = Node (board, maximum players) trees'
                                where
                                    trees'  = map miniMax trees
                                    players = [player | Node (_,player) _ <- trees'] 

-- A list of best moves for a player given a specific board and calculate depth of each paths
bestMoveDepths :: Player -> Board -> [(Depth,Board)]
bestMoveDepths player board = 
    [(depth board',board') | Node (board',player') branches' <- branches, player' == best]
        where
            tree :: Tree Board
            tree                   = pruneTree _TREE_DEPTH_ (boardTree player board)
            Node (_,best) branches = miniMax tree
            depth :: Board -> Int
            depth b                = pathDepth $ boardTree player b

-- Find the best move by calculating the shortest path from the best moves
playComputerMove :: Player -> Board -> IO (GameState, Board)
playComputerMove player board = return (getGameState board', board')
                                where
                                    quickpath = minimum $ bestMoveDepths player board
                                    board'    = snd quickpath