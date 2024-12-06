import Data.List
import System.Environment

data Direction = U | R | D | L deriving (Eq)
type Loc = (Int, Int)
type Grid = [[Char]]

rotateDirection :: Direction -> Direction
rotateDirection U = R
rotateDirection R = D
rotateDirection D = L
rotateDirection L = U

stepLoc :: Loc -> Direction -> Loc
stepLoc (l, c) U = (l - 1, c)
stepLoc (l, c) R = (l, c + 1)
stepLoc (l, c) D = (l + 1, c)
stepLoc (l, c) L = (l, c - 1)

inBounds (l, c) grid = l >= 0 && l < length grid && c >= 0 && c < length (grid !! l)

traverseGrid :: Grid -> Loc -> Direction -> [Loc]
traverseGrid grid loc d
    | inBounds loc' grid = loc : traverseGrid grid loc' d'
    | otherwise = [loc]
  where
    (loc'@(l, c), d') = stepGrid grid loc d

stepGrid :: Grid -> Loc -> Direction -> (Loc, Direction)
stepGrid g s d = if inBounds s' g && g !! l' !! c' == '#' then stepGrid g s (rotateDirection d) else (s', d)
  where
    s'@(l', c') = stepLoc s d

isGoodObstacle :: Loc -> Grid -> Loc -> Bool
isGoodObstacle start grid (l, c) = isGoodObstacle' (start, U) g' []
  where
    (x, v : y) = splitAt l grid
    (x', _ : y') = splitAt c v
    g' = x ++ (x' ++ '#' : y') : y
    isGoodObstacle' :: (Loc, Direction) -> Grid -> [(Loc, Direction)] -> Bool
    isGoodObstacle' l g p = l `elem` p || (inBounds (fst l) g && isGoodObstacle' (uncurry (stepGrid g) l) g (l : p))

sameTrack :: (Loc, Direction) -> (Loc, Direction) -> Bool
sameTrack ((x, a), U) ((y, b), U) = a == b && x >= y
sameTrack ((x, a), D) ((y, b), D) = a == b && x <= y
sameTrack ((a, x), R) ((b, y), R) = a == b && x <= y
sameTrack ((a, x), L) ((b, y), L) = a == b && x >= y

findObstacleSpots :: Grid -> Loc -> Direction -> [(Loc, Direction)] -> [Loc]
findObstacleSpots grid loc d p
    | inBounds loc' grid =
        if any (sameTrack (loc', rotateDirection d')) $ filter (\(_, b) -> b == rotateDirection d') p
            then loc'' : findObstacleSpots grid loc' d' p'
            else findObstacleSpots grid loc' d' p'
    | otherwise = []
  where
    (loc'@(l, c), d') = stepGrid grid loc d
    (loc'', d'') = stepGrid grid loc' d'
    p' = (loc', d') : p

main = do
    [fileLoc] <- getArgs
    file <- lines <$> readFile fileLoc
    -- Part 1
    let (Just l) = findIndex ('^' `elem`) file
        (Just c) = elemIndex '^' (file !! l)
        start = (l, c)
        path = nub $ start : traverseGrid file start U
    print $ length path

    -- Part 2
    -- 1304
    -- print $ length $ findObstacleSpots file start U []
    let obs' = filter (isGoodObstacle start file) $ tail path
    print $ length obs'
