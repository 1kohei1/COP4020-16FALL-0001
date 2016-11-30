-- module 8: Type synonyms
import qualified Data.Map as Map

data LockerState = Taken | Free deriving (Show, Eq)

type Code = String

type LockerMap = Map.Map Int (LockerState, Code)

lockers :: LockerMap  
lockers = Map.fromList   [
    (100, (Taken, "ZD39I")),
    (101, (Free,  "JAH3I")),
    (103, (Free,  "IQSA9")),
    (105, (Free,  "QOTSA")),
    (109, (Taken, "893JJ")),
    (110, (Taken, "99292"))  
    ] 

lockerLookup:: Int -> LockerMap -> Either String Code 
lockerLookup lockerNumber map = 
    case Map.lookup lockerNumber map of 
        Nothing -> Left $ "Locker Number " ++ show lockerNumber ++ " doesn't exist!"
        Just (state, code) -> if state /= Taken
                              then Right code
                              else Left $ "Locker " ++ show lockerNumber ++ " is already taken!"

-- Recursive data structures
data List a  = Empty | Cons a (List a) deriving (Show, Read, Eq, Ord)
-- Cons represents : as we can use function like 2 `elem` [1,2,3]. 
-- This is another notation of :


