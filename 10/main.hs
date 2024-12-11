import Data.Char
import Data.List
import System.Environment

type Loc = (Int, Int)
data Direction = L | R | U | D
type Grid = [[Int]]

stepLoc :: Loc -> Direction -> Loc
stepLoc (l, c) U = (l - 1, c)
stepLoc (l, c) R = (l, c + 1)
stepLoc (l, c) D = (l + 1, c)
stepLoc (l, c) L = (l, c - 1)

inBounds (l, c) grid = l >= 0 && l < length grid && c >= 0 && c < length (grid !! l)

reachableNines :: Grid -> Loc -> Int -> [Loc]
reachableNines grid loc@(l, c) prev
    | not $ inBounds loc grid = []
    | grid !! l !! c == prev + 1 = ([loc | prev + 1 == 9]) ++ concatMap (\d -> reachableNines grid (stepLoc loc d) (grid !! l !! c)) dirs
    | otherwise = []
  where
    dirs = [U, D, L, R]

main :: IO ()
main = do
    [fileloc] <- getArgs
    grid <- map (map digitToInt) . lines <$> readFile fileloc
    let zeros = concat $ filter (not . null) $ zipWith (\l i -> map (i,) $ elemIndices 0 l) grid [0 ..]
        x = map (\l -> reachableNines grid l (-1)) zeros
    -- Part 1
    print $ sum $ map (length . nub) x
    -- Part 2
    print $ sum $ map length x
