--
--  Group 24 HW1
--    Members:
--      Kohei Arai
--      Sandra Hoopes
--      Alexander Lascaibar
--      Victoria Proetsch
--      David Waters
--
--  Problem 1
computePolygonArea :: [(Double, Double)] -> Double
computePolygonArea [] = error "No points in the list. ERROR!!"
computePolygonArea [(_, _)] = error "Only one point. ERROR!!"
-- What if there are only two points? Isn't that also another error case since there is no area?
-- When it reaches here, it is guaranteed to have two elements in the list.
computePolygonArea all@(a:b:[]) = computeRecursive all / 2
computePolygonArea all@(a:b:x) = (det (last x) a + computeRecursive all) / 2

computeRecursive :: [(Double, Double)] -> Double
computeRecursive (a:b:[]) = det a b
computeRecursive (a:b:x) = det a b + computeRecursive (b:x)

det :: (Double,Double) -> (Double,Double) -> Double
det (a, b) (c, d) = a * d - b * c

-- Problem 2
-- Learn how to declare custom type
module Chess where
import Data.Char

-- See https://en.wikipedia.org/wiki/Chess for more details
-- We only consider the situation where there is only a single
-- piece on the board

-- see Rules - Set up for basic definitions

type File     = Char         -- column index
                             -- valid files are 'a','b',...,'h'
type Rank     = Int          -- row index
                             -- valid ranks are 1,2,...8
type Position = (File,Rank)

data Color =
  Black | White
  deriving (Eq,Show)

data Piece =
  King | Queen | Rook | Bishop | Knight | Pawn
  deriving (Eq,Show)

-- Takes in pair, then checks if x and y are valid positions on the board.
-- Returns true if it is, otherwise false.
isLegalPosition :: Position -> Bool
isLegalPosition (x, y) = x `elem` ['a'..'h'] && y `elem` [1..8]

-- Gets a color, piece type, and both a start and end position. This calls the
-- helper function isLegalMove' and returns it's boolean value.
isLegalMove :: Color -> Piece -> Position -> Position -> Bool
isLegalMove color piece start end = (isLegalPosition start && isLegalPosition end) &&
  isLegalMove' color piece start end

-- Deciphers which function to call in order to validate the move. All functions
-- that are called return a boolean value that represents if the move is allowed.
isLegalMove' :: Color -> Piece -> Position -> Position -> Bool
isLegalMove' _ King start end     = isValidKingMove start end
isLegalMove' _ Queen start end    = isValidQueenMove start end
isLegalMove' _ Rook start end     = isValidRookMove start end
isLegalMove' _ Knight start end   = isValidKnightMove start end
isLegalMove' _ Bishop start end   = isValidBishopMove start end
isLegalMove' White Pawn start end = isValidWhitePawnMove start end
isLegalMove' Black Pawn start end = isValidBlackPawnMove start end

-- For a king, it can move is only space in any direction. (Box)
isValidKingMove :: Position -> Position -> Bool
isValidKingMove (startColumn, startRow) (endColumn, endRow)
  | startColumn == endColumn && startRow == endRow = False
  | abs rowDiff <= 1 && abs colDiff <= 1           = True
  | otherwise                                      = False
  where rowDiff                                    = startRow - endRow
        colDiff                                    = Data.Char.ord startColumn - Data.Char.ord endColumn

-- For a queen, it can move any number of spaces along rank, file, or diagonally. (Star)
isValidQueenMove :: Position -> Position -> Bool
isValidQueenMove (startColumn, startRow) (endColumn, endRow)
  | startColumn == endColumn && startRow == endRow = False
  | otherwise = isValidBishopMove (startColumn, startRow) (endColumn, endRow) ||
                  isValidRookMove (startColumn, startRow) (endColumn, endRow)

-- For a rook, they can move any number of spaces in along any rank or file. (Cross)
isValidRookMove :: Position -> Position -> Bool
isValidRookMove (startColumn, startRow) (endColumn, endRow)
  | startColumn == endColumn && startRow == endRow = False
  | rowDiff == 0 && colDiff /= 0                   = True
  | colDiff == 0 && rowDiff /= 0                   = True
  | otherwise                                      = False
  where rowDiff                                    = startRow - endRow
        colDiff                                    = Data.Char.ord startColumn - Data.Char.ord endColumn

-- For a knight, they can move two veritcally and one horizontally or two horizontally and one vertically. (L-shape)
isValidKnightMove :: Position -> Position -> Bool
isValidKnightMove (startColumn, startRow) (endColumn, endRow)
  | startColumn == endColumn && startRow == endRow = False
  | abs result == 2                                = True
  | otherwise                                      = False
  where result                                     = (Data.Char.ord startColumn - Data.Char.ord endColumn) * (startRow - endRow)

-- For a bishop, they can move any number of spaces diagonally. (X-shape)
isValidBishopMove :: Position -> Position -> Bool
isValidBishopMove (startColumn, startRow) (endColumn, endRow)
  | startColumn == endColumn && startRow == endRow = False
  | abs rowDiff == abs colDiff                     = True
  | otherwise                                      = False
  where rowDiff                                    = startRow - endRow
        colDiff                                    = Data.Char.ord startColumn - Data.Char.ord endColumn

-- For a Pawn, they can move one space forward if available. That applies generally unless 
-- it is the pawns first move from starting position, in which case it can move two.
isValidWhitePawnMove :: Position -> Position -> Bool
isValidWhitePawnMove (startColumn, startRow) (endColumn, endRow)
  | startColumn == endColumn && startRow == endRow = False
  | startColumn /= endColumn                       = False
  | startRow == 1                                  = False
  | endRow - startRow == 1                         = True
  | startRow == 2 && endRow == 4                   = True
  | otherwise                                      = False

-- Same a white Pawn.
isValidBlackPawnMove :: Position -> Position -> Bool
isValidBlackPawnMove (startColumn, startRow) (endColumn, endRow)
  | startColumn == endColumn && startRow == endRow = False
  | startColumn /= endColumn     = False
  | startRow == 8                = False
  | startRow - endRow == 1       = True
  | startRow == 7 && endRow == 5 = True
  | otherwise                    = False