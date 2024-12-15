import Control.Monad
import Control.Monad.ST
import Data.List
import Data.STRef
import System.Environment

data Dir = U | D | L | R deriving (Show)
parseDir :: Char -> Dir
parseDir '^' = U
parseDir 'v' = D
parseDir '<' = L
parseDir '>' = R

type Coord = (Int, Int)
type Env = ([Coord], [Coord], Coord)

step :: Coord -> Dir -> Coord
step (x, y) U = (x, y - 1)
step (x, y) D = (x, y + 1)
step (x, y) L = (x - 1, y)
step (x, y) R = (x + 1, y)

parseGrid :: [String] -> Env -- (Boxes, Walls, Robot)
parseGrid s = foldr f ([], [], (0, 0)) ss
  where
    ss = zip (map (\s' -> zip s' [0 ..]) s) [0 ..]
    f (s, y) acc = foldr g acc s
      where
        g ('O', x) (b, w, r) = ((x, y) : b, w, r)
        g ('#', x) (b, w, r) = (b, (x, y) : w, r)
        g ('@', x) (b, w, _) = (b, w, (x, y))
        g ('.', _) v = v

type Area = (Coord, Coord) -- (Top-left, bottom-right)

gpsCalc :: Coord -> Int
gpsCalc (x, y) = 100 * y + x

scaleWidth :: Int -> Coord -> Coord
scaleWidth i (x, y) = (x * i, y)

gpsCalcScaled :: Area -> Int
gpsCalcScaled (l, _) = gpsCalc l

runMovesFolding :: a -> [Dir] -> (a -> Dir -> a) -> a
runMovesFolding env moves f = runST $ do
    acc <- newSTRef env
    forM_ moves $ \x -> do
        acc' <- readSTRef acc
        writeSTRef acc (f acc' x)
    readSTRef acc

singleSized :: Env -> Dir -> Env
singleSized acc@(b, w, r) x = if canPush (b, w, r') x then (push b r' x, w, r') else acc
  where
    r' = step r x
    canPush (b, w, r@(x, y)) d = r `notElem` w && (r `notElem` b || canPush (b, w, step r d) d)
    push b p x = case find (== p) b of
        Just p' -> step p' x : push (delete p b) (step p x) x
        Nothing -> b

doubleWidth acc@(b, w, r) x = if canPush (b, w, r') x then (push b r' x, w, r') else acc
  where
    r' = step r x
    canPush (b, w, r) d =
        not (any (hits r) w) && case find (hits r) b of
            Just a@(x', y') -> let b' = delete a b in canPush (b', w, step x' d) d && canPush (b', w, step y' d) d
            Nothing -> True
    push b p d = case find (hits p) b of
        Just p'@(x', y') -> let b' = delete p' b in stepArea p' d : push (push b' (step x' d) d) (step y' d) d
        Nothing -> b
    hits p (a, b) = p == a || p == b
    stepArea (a, b) d = (step a d, step b d)

main :: IO ()
main = do
    [fileloc] <- getArgs
    (grid, moves') <- (\i -> (takeWhile (not . null) i, filter (/= '\n') $ init $ unlines $ drop 1 $ dropWhile (not . null) i)) . lines <$> readFile fileloc
    -- Part 1
    let moves = map parseDir moves'
        env@(boxes, walls, robotInit) = parseGrid grid
        (bGen, _, _) = runMovesFolding env moves singleSized
    print $ sum $ map gpsCalc bGen
    -- Part 2
    let b' = map ((\r@(x, y) -> (r, (x + 1, y))) . scaleWidth 2) boxes
        w' = map ((\r@(x, y) -> (r, (x + 1, y))) . scaleWidth 2) walls
        r' = scaleWidth 2 robotInit
        env' = (b', w', r')
        (bGen', _, _) = runMovesFolding env' moves doubleWidth
    print $ sum $ map gpsCalcScaled bGen'
