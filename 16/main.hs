import Data.List
import Data.Set qualified as S
import System.Environment

type Coord = (Int, Int)

data Dir = U | R | D | L deriving (Show, Eq, Ord)

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

getDir :: Coord -> Coord -> Dir
getDir (a, b) (x, y)
    | a == x && (b - y) == 1 = D
    | a == x && (y - b) == 1 = U
    | b == y && (a - x) == 1 = R
    | b == y && (x - a) == 1 = L

getDirCost :: Dir -> Dir -> Int
getDirCost a b
    | a == b = 0
    | a == inverseDir b = 2000
    | otherwise = 1000

dirs = [R, D, U, L]

step :: Coord -> Dir -> Coord
step (x, y) U = (x, y - 1)
step (x, y) D = (x, y + 1)
step (x, y) L = (x - 1, y)
step (x, y) R = (x + 1, y)

getAllPaths :: Coord -> Coord -> S.Set Coord -> S.Set Coord -> [[Coord]]
getAllPaths curr target p prev
    | S.null p = []
    | curr == target = [[curr]]
    | otherwise = map (curr :) $ concatMap (\n -> getAllPaths n target p (S.insert curr prev)) nxts
  where
    nxts = filter (`elem` p) $ filter (`notElem` prev) $ map (step curr) dirs
getPathCost :: Dir -> [Coord] -> Int
getPathCost d (x : xs) = getPathCost' x d xs
getPathCost' :: Coord -> Dir -> [Coord] -> Int
getPathCost' _ _ [] = 0
getPathCost' curr from (x : xs) = 1 + dirCost + getPathCost' x to xs
  where
    to = getDir curr x
    dirCost = getDirCost to from

parseGrid :: [String] -> ([Coord], Coord, Coord) -- Points, start, end
parseGrid s = foldr f ([], (0, 0), (0, 0)) ss
  where
    ss = zip (map (\s' -> zip s' [0 ..]) s) [0 ..]
    f (s, y) acc = foldr g acc s
      where
        g ('.', x) (p, s, e) = ((x, y) : p, s, e)
        g ('#', _) v = v
        g ('S', x) (p, _, e) = ((x, y) : p, (x, y), e)
        g ('E', x) (p, s, _) = ((x, y) : p, s, (x, y))

getAllPathsCosts :: Dir -> Coord -> Coord -> S.Set Coord -> S.Set Coord -> [Int]
getAllPathsCosts dir curr target p prev
    | S.null p = []
    | curr == target = [0]
    | otherwise = concatMap (\(n, d) -> map (+ (1 + getDirCost dir d)) $ getAllPathsCosts d n target p (S.insert curr prev)) nxts
  where
    nxts = filter ((`elem` p) . fst) $ filter ((`notElem` prev) . fst) $ map (\d -> (step curr d, d)) dirs

main :: IO ()
main = do
    [fileloc] <- getArgs
    grid@(g, s, e) <- parseGrid . lines <$> readFile fileloc
    let paths' = getAllPaths s e (S.fromList g) S.empty
    -- Part 1
    print $ minimum $ map (getPathCost L) paths'
    -- print $ getAllPathsCosts L s e (S.fromList g) S.empty
    -- Part 1
    print $ S.size $ S.fromList $ concatMap snd $ head $ groupBy (\(a, _) (b, _) -> a == b) $ sortOn fst $ map (\p -> (getPathCost L p, p)) paths'
