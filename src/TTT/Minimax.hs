module TTT.Minimax where

{-
The MiniMax Algorithm
This is an extra module outside of the regular haskell-dojo course.
This implements a version of tictactoe to play against the  computer by using the minimax algorithm. 
ACKNOWLEDGEMNT: Graham Hutton from Programming in Haskell for explaining the algorithm and providing 
an example.
-}

import TTT.A1
import TTT.A2 ( _EMPTY_BOARD_ )
import TTT.A3 ( isValidMove )
import TTT.A4 ( getGameState, playMove, showSquares )


-------------------- Generic Tree Data Structure and Functions --------------------


{-
A tree structure for the board tree
Comprises of a node root of an element and branches derived from the root made up of subtrees. 
Instead of having a separate data constructor (Leaf | Node a [subtrees]for Leaf, Leaf is a  node
with no subtree: Leaf == Node a []
-}
data Tree a = Node a [Tree a]
                deriving Show


-- A type synonym for distance between root and leaves of branches 
type Depth = Int

-- A variable to limit depth of tree to be calculated to limit time and memory
_TREE_DEPTH_ :: Int
_TREE_DEPTH_ = 9

-- A function to prune the tree to a certain depth
pruneTree :: Int -> Tree a -> Tree a
pruneTree 0 (Node root _)        = Node root []
pruneTree n (Node root branches) = Node root [pruneTree (n-1) branch | branch <- branches]

-- Calculate the depth of node from root to leaves. note this is inclusive of all branches
pathDepth :: Tree a -> Depth
pathDepth (Node root [])       = 1
pathDepth (Node root branches) = 1 + sum [pathDepth branch | branch <- branches]


------------------------ TicTacToe Minimax Algorithm Implementation ----------------- 


-- A board tree that derives all possible board configurations (branches) from player's current node 
boardTree :: Player -> Board -> Tree Board
boardTree player board =
    Node board [boardTree (switchPlayer player) board' | board' <- getAllValidBoards player board]


-- All possible moves of size _SIZE_
-- (This ould've been done purely with Int's but implemented instead with the spirit of the haskell-dojo 
-- assignments
getAllMoves :: [Move]
getAllMoves = [(x,y) | x <- rows, y <- cols]
                where
                    rows :: [Int]
                    rows = [0 .. _SIZE_ - 1]
                    cols :: [Int]
                    cols = [convertRowIndex c | c <- take _SIZE_ ['a' ..]]

-- Generate all possible board configurations by applying all valid moves by player to current board                    
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
nextPlayer board = if xs > os then O else X
                    where
                        xs :: Int
                        xs      = length $ filter (==X) squares 
                        os :: Int
                        os      = length $ filter (==O) squares
                        squares :: [Player]
                        squares = concat board

{- 
Minimax Algorithm
NOTE: The Square data structure needs to be ordered: 
        data Square O | E | X deriving (Show, Eq, Ord)
Ord ordering therefore produces O < E < X. This is to ensure calculating minimized O and maximized X
-}
miniMax :: Tree Board -> Tree (Board, Player)
miniMax (Node board []) =
    case getGameState board of
        X_Wins      -> Node (board,X) []
        O_Wins      -> Node (board,O) []
        Is_Draw     -> Node (board,E) []
        In_Progress -> Node (board,E) []
miniMax (Node board trees) 
    | nextPlayer board == X = Node (board, maximum players) trees'
    | nextPlayer board == O = Node (board, minimum players) trees'
                                where
                                    trees'  = map miniMax trees
                                    players = [player | Node (_,player) _ <- trees'] 

-- A list of best moves (board configuration) for a player given a specific board 
-- with the depth calculated
-- -> generate player's board tree -> minimax the tree -> calculate depth of each best move
bestMoveDepths :: Player -> Board -> [(Depth,Board)]
bestMoveDepths player board = 
    [(depth board',board') | Node (board',player') branches' <- branches, player' == best]
        where
            tree :: Tree Board
            tree                   = pruneTree _TREE_DEPTH_ (boardTree player board)
            Node (_,best) branches = miniMax tree
            depth :: Board -> Depth
            depth board''          = pathDepth $ boardTree player board''

-- Find the best move by calculating the shortest path from the best moves
-- The main function to play the game
playComputerMove :: Player -> Board -> IO (GameState, Board)
playComputerMove player board = return (getGameState board', board')
                                where
                                    quickpath = minimum $ bestMoveDepths player board
                                    board'    = snd quickpath