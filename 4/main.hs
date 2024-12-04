import Data.List
import System.Environment

data Direction = North | NorthEast | East | SouthEast | South | SouthWest | West | NorthWest

type Grid a = [[a]]
type Loc = (Int, Int) -- (LINE, CHAR)

step :: Loc -> Direction -> Loc
step (l, c) North = (l - 1, c)
step (l, c) NorthEast = (l - 1, c + 1)
step (l, c) East = (l, c + 1)
step (l, c) SouthEast = (l + 1, c + 1)
step (l, c) South = (l + 1, c)
step (l, c) SouthWest = (l + 1, c - 1)
step (l, c) West = (l, c - 1)
step (l, c) NorthWest = (l - 1, c - 1)

findXmas :: [String] -> Loc -> Int
findXmas content i = length $ filter (\d -> findSequence content "MAS" (step i d) d) dirs
  where
    dirs = [North, NorthEast, East, SouthEast, South, SouthWest, West, NorthWest]

findSequence :: (Eq a) => Grid a -> [a] -> Loc -> Direction -> Bool
findSequence _ [] _ _ = True
findSequence content (s : search) (l, c) dir = bounds && content !! l !! c == s && findSequence content search (step (l, c) dir) dir
  where
    bounds = l >= 0 && l < length content && c >= 0 && c < length (content !! l)

findX :: [String] -> Loc -> Bool
findX content i = any downSearch s && any upSearch s
  where
    s = ["MAS", "SAM"]
    downSearch s = findSequence content s (step i NorthWest) SouthEast
    upSearch s = findSequence content s (step i NorthEast) SouthWest

-- \|| findSequence content s (step i SouthEast) NorthWest

main = do
    [fileLoc] <- getArgs
    file <- lines <$> readFile fileLoc
    let findEl s f = concatMap (\(a, b) -> map (a,) b) $ zipWith (\a b -> (a, elemIndices s b)) [0 ..] f
    -- Part 1
    let xs :: [Loc]
        xs = findEl 'X' file
    print $ sum $ map (findXmas file) xs
    -- Part 2
    let xs' :: [Loc]
        xs' = findEl 'A' file
    print $ length $ filter (findX file) xs'
