import Data.List
import System.Environment

findXmas :: [String] -> (Int, Int) -> Int
findXmas content i = length $ filter (findSequence content "XMAS" i) dirs
  where
    dirs = [North, NorthEast, East, SouthEast, South, SouthWest, West, NorthWest]

data Direction = North | NorthEast | East | SouthEast | South | SouthWest | West | NorthWest

findSequence :: (Eq a) => [[a]] -> [a] -> (Int, Int) -> Direction -> Bool
findSequence _ [] _ _ = True
findSequence content (s : search) (l, c) direction
    | l >= 0 && l < length content && c >= 0 && c < length (content !! l) && content !! l !! c == s = findSequence content search newIndex direction
    | otherwise = False
  where
    newIndex = case direction of
        North -> (l - 1, c)
        NorthEast -> (l - 1, c + 1)
        East -> (l, c + 1)
        SouthEast -> (l + 1, c + 1)
        South -> (l + 1, c)
        SouthWest -> (l + 1, c - 1)
        West -> (l, c - 1)
        NorthWest -> (l - 1, c - 1)

findX :: [String] -> (Int, Int) -> Bool
findX content i =
    (findLetters "MS" || findLetters "SM")
        && (findLetters' "MS" || findLetters' "SM")
  where
    findLetters [a, b] = findSequence content ['A', a] i NorthWest && findSequence content ['A', b] i SouthEast
    findLetters' [a, b] = findSequence content ['A', a] i NorthEast && findSequence content ['A', b] i SouthWest

main = do
    [fileLoc] <- getArgs
    file <- lines <$> readFile fileLoc
    -- Part 1
    let xs :: [(Int, Int)] -- (LINE, CHAR)
        xs = concatMap (\(a, b) -> map (a,) b) $ zipWith (\a b -> (a, elemIndices 'X' b)) [0 ..] file
    print $ sum $ map (findXmas file) xs
    -- Part 2
    let xs' :: [(Int, Int)] -- (LINE, CHAR)
        xs' = concatMap (\(a, b) -> map (a,) b) $ zipWith (\a b -> (a, elemIndices 'A' b)) [0 ..] file
    print $ length $ filter (findX file) xs'
