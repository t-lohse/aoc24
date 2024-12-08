import Control.Applicative
import Data.List
import Data.Map qualified as M
import System.Environment

data Point = Point Int Int deriving (Eq, Show)
instance Num Point where
    Point a b + Point x y = Point (a + x) (b + y)
    Point a b * Point x y = Point (a * x) (b * y)
    abs (Point a b) = Point (abs a) (abs b)
    fromInteger i = Point (fromInteger i) (fromInteger i)
    signum (Point a b)
        | a < 0 && b < 0 = -1
        | a > 0 && b > 0 = 1
        | otherwise = 0
    (Point a b) - (Point x y) = Point (a - x) (b - y)

type Grid a = [[a]]

parseGrid :: Grid Char -> M.Map Char [Point]
parseGrid grid = foldl (M.unionWith (++)) M.empty $ zipWith (\s line -> foldl (\acc (c, ch) -> if c /= '.' then M.alter (h line ch) c acc else acc) M.empty $ zip s [0 ..]) grid [0 ..]
  where
    h line char v = let p = Point line char in fmap (p :) v <|> return [p]

computeAntinodes :: M.Map Char [Point] -> [Point]
computeAntinodes = concatMap f
  where
    f ps = [a - (b - a) | a <- ps, b <- ps, a /= b]

computeAntinodesTraj :: Int -> M.Map Char [Point] -> [Point]
computeAntinodesTraj w = concatMap f
  where
    f ps = concat [h a (b - a) | a <- ps, b <- ps, a /= b]
    h src a = if inBounds w src then src : h (src - a) a else []

inBounds :: Int -> Point -> Bool
inBounds c (Point a b) = not (a < 0 || b < 0 || a >= c || b >= c)

main = do
    [fileloc] <- getArgs
    file <- lines <$> readFile fileloc
    let width = length file
        myMap = parseGrid file
    -- Part 1
    print $ length $ nub $ filter (inBounds width) $ computeAntinodes myMap
    -- Part 2
    print $ length $ nub $ computeAntinodesTraj width myMap
