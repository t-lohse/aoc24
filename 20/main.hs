import Control.Applicative
import Data.IntMap qualified as IM
import Data.List
import System.Environment

data Dir = U | D | L | R deriving (Show, Eq, Ord)
type Coord = (Int, Int)
type Grid a = [[a]]
type Path = [Coord]

inverseDir :: Dir -> Dir
inverseDir U = D
inverseDir R = L
inverseDir D = U
inverseDir L = R

clockWise :: Dir -> Dir
clockWise U = R
clockWise R = D
clockWise D = L
clockWise L = U

counterClockWise :: Dir -> Dir
counterClockWise = clockWise . inverseDir

step :: Coord -> Dir -> Coord
step (x, y) U = (x, y - 1)
step (x, y) D = (x, y + 1)
step (x, y) L = (x - 1, y)
step (x, y) R = (x + 1, y)

(!) :: Grid a -> Coord -> a
grid ! c@(x, y)
    | x >= 0 && y >= 0 = grid !! y !! x
    | otherwise = error $ show c

inBounds :: Int -> Coord -> Bool
inBounds c (x, y) = x >= 0 && y >= 0 && x < c && y < c

findStartEnd :: Grid Char -> (Coord, Coord)
findStartEnd s = foldr f ((0, 0), (0, 0)) ss
  where
    ss = zip (map (\s' -> zip s' [0 ..]) s) [0 ..]
    f (s, y) acc = foldr g acc s
      where
        g ('S', x) (_, end) = ((x, y), end)
        g ('E', x) (start, _) = (start, (x, y))
        g (_, _) v = v

findPath :: Grid Char -> Coord -> Coord -> Path
findPath grid end start = start : goThrough grid end nxt d
  where
    (nxt, d)
        | grid ! step start U == '.' = (step start U, U)
        | grid ! step start D == '.' = (step start D, D)
        | grid ! step start L == '.' = (step start L, L)
        | grid ! step start R == '.' = (step start R, R)

goThrough :: Grid Char -> Coord -> Coord -> Dir -> Path
goThrough grid target curr dir
    | target == curr = [target]
    | otherwise = curr : goThrough grid target nxt dir'
  where
    (nxt, dir')
        | grid ! n `elem` ".E" = (n, dir)
        | grid ! l `elem` ".E" = (l, left)
        | grid ! r `elem` ".E" = (r, right)
        | otherwise = error "no way"
    left = counterClockWise dir
    right = clockWise dir
    n = step curr dir
    l = step curr left
    r = step curr right

getSkipCounts :: Int -> Path -> IM.IntMap Int
getSkipCounts _ [] = IM.empty
getSkipCounts i (p@(x, y) : ps) = IM.unionWith (+) res2 $ getSkipCounts i ps
  where
    res2 = foldr g IM.empty ps
    g p' res =
        let i' = diff p'
            (Just ii) = elemIndex p' ps
         in if i' <= i then IM.insertWith (+) (ii - i' + 1) 1 res else res
    diff (x', y') = abs (x - x') + abs (y - y')

main :: IO ()
main = do
    [fileLoc] <- getArgs
    grid <- lines <$> readFile fileLoc
    let (start, end) = findStartEnd grid
        normalPath = findPath grid end start
        normalSteps = length normalPath
    -- Part 1
    print $ sum $ IM.filterWithKey (\k _ -> k >= 100) $ getSkipCounts 2 normalPath
    -- Part 2
    print $ sum $ IM.filterWithKey (\k _ -> k >= 100) $ getSkipCounts 20 normalPath
