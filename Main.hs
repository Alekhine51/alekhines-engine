module Main where

import qualified Data.Vector as V
import qualified Chess as C
import Data.Maybe


main :: IO ()
main = putStrLn "Hello, Haskell!"


data PieceType = King | Queen | Rook | Knight | Bishop | Pawn deriving (Eq, Show)
data Color = White | Black deriving (Eq, Show)
data Piece = Piece { color :: Color, pieceType :: PieceType} deriving (Eq, Show)

-- TODO: Implement move enumeration for each piece (how to not go off board?) (Use a tree graph to store paths)
-- TODO: Implement Board evaluation

data Board = 
    Board 
    { board :: V.Vector (Maybe Piece), -- the actual board 
    kw :: Bool, -- white kingside castle right, and so on
    qw :: Bool,
    kb :: Bool,
    qb :: Bool,
    moves :: Int, -- number of moves, incremented after Black moves
    enPassant :: Maybe Int, -- hold the square 'behind' a pawn after it makes a two-step move
    activeColor :: Color -- side to move
    } deriving (Eq, Show) 

-- function to construct starting board.
-- n is the index of the element
setBoard :: Int -> Maybe Piece
setBoard n 
    | (n == 0 || n == 7) = Just $ Piece White Rook
    | (n == 1 || n == 6) = Just $ Piece White Knight
    | (n == 2 || n == 5) = Just $ Piece White Bishop
    | (n == 3) = Just $ Piece White Queen 
    | (n == 4) = Just $ Piece White King 
    | (n >= 8 && n <= 15) = Just $ Piece White Pawn 
    | (n >= 48 && n <= 55) = Just $ Piece Black Pawn 
    | (n == 56 || n == 63) = Just $ Piece Black Rook 
    | (n == 57 || n == 62) = Just $ Piece Black Knight 
    | (n == 58 || n == 61) = Just $ Piece Black Bishop 
    | (n == 59) = Just $ Piece Black Queen
    | (n == 60) = Just $ Piece Black King 
    | otherwise = Nothing
            

startingBoard = V.generate 64 setBoard

initialFullBoard = Board startingBoard True True True True 0 Nothing White


-- translates an index into 2D coordinates
-- Dont actually use these, this is really inefficient
coord :: Int -> (Int, Int)
coord index = (mod index 8, div index 8)

-- translate 2D coordinates into an index
deCoord :: Num a => (a, a) -> a
deCoord (x, y) = (y * 8) + x

isBlack piece = color piece == Black
isWhite piece = color piece == White

-- enumerate each possible move as a new board state, and store them all in a list
enumerateMoves boardState = undefined

 -- for a given pawn, enumerate all possible moves
-- enumeratePawnMoves index color boardState = if color == White && index >= 8 && index <= 15 then makeMove (board boardState) piece (index + 2) else 





isEnemy :: Piece -> Piece -> Bool
isEnemy attacker defender = color attacker /= color defender

-- directions
up index x = index + (8 * x)
down index x = index - (8 * x)
right index x = index + x
left index x = index - x

-- Board, moving piece, index, destination, new board state
-- replaces index with `Nothing`
makeMove :: V.Vector (Maybe a) -> Maybe a -> Int -> Int -> V.Vector (Maybe a)
makeMove board piece index dest =  board V.// [(dest, piece), (index, Nothing)]


-- find all pieces in range of this rook 
rookRange index boardState = undefined


-- evaluate how favorable the board is for a certain color
evaluateBoard board color = undefined

-- evaluate a single piece
evaluatePiece board index = undefined

-- evaluate single type of piece

evaluatePawn board color index = undefined

-- check if a pawn is passed
isPassed board index color = undefined 
