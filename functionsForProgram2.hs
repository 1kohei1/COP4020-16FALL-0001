isValidWhitePawnMove :: Position -> Position -> Bool
isValidWhitePawnMove (startColumn, startRow) (endColumn, endRow)
  | startColumn == endColumn && startRow == endRow = False
  | startColumn != endColumn                       = False
  | startRow = 1                                   = False
  | endRow - startRow == 1                         = True
  | startRow == 2 && endRow == 4                   = True
  | otherwise                                      = False

isValidBlackPawnMove :: Position -> Position -> Bool
isValidBlackPawnMove (startColumn, startRow) (endColumn, endRow)
  | startColumn == endColumn && startRow == endRow = False
  | startColumn != endColumn     = False
  | startRow == 8                = False
  | startRow - endRow == 1       = True
  | startRow == 7 && endRow == 5 = True
  | otherwise                    = False

isValidKnightMove :: Position -> Position -> Bool
isValidKnightMove (startColumn, startRow) (endColumn, endRow)
  | startColumn == endColumn && startRow == endRow = False
  | abs result == 2                                = True
  | otherwise                                      = False
  where result                                     = (Data.Char.ord startColumn - Data.Char.ord endColumn) * (startRow - endRow)

isValidQueenMove :: Position -> Position -> Bool
isValidQueenMove (startColumn, startRow) (endColumn, endRow)
  | startColumn == endColumn && startRow == endRow = False
  | otherwise = isValidBishopMove (startColumn, startRow) (endColumn, endRow) || isValidRookMove (startColumn, startRow) (endColumn, endRow)

isValidBishopMove :: Position -> Position -> Bool
isValidBishopMove (startColumn, startRow) (endColumn, endRow)
  | startColumn == endColumn && startRow == endRow = False
  | abs rowDiff == abs colDiff                     = True
  | otherwise                                      = False
  where rowDiff                                    = startRow - endRow
        colDiff                                    = Data.Char.ord startColumn - Data.Char.ord endColumn

isValidRookMove :: Position -> Position -> Bool
isValidRookMove (startColumn, startRow) (endColumn, endRow)
  | startColumn == endColumn && startRow == endRow = False
  | rowDiff == 0 && colDiff != 0                   = True
  | colDiff == 0 && rowDiff != 0                   = True
  | otherwise = False
  where rowDiff                                    = startRow - endRow
        colDiff                                    = Data.Char.ord startColumn - Data.Char.ord endColumn


isValidKingMove :: Position -> Position -> Bool
isValidKingMove (startColumn, startRow) (endColumn, endRow)
  | startColumn == endColumn && startRow == endRow = False
  | abs rowDiff <= 1 && abs colDiff <= 1                   = True
  | otherwise                                      = False
  where rowDiff                                    = startRow - endRow
        colDiff                                    = Data.Char.ord startColumn - Data.Char.ord endColumn
