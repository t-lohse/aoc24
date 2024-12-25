import Data.List
import Data.Map qualified as M
import Graph.DijkstraSimple
import Graph.DijkstraSimple.Weighters
import System.Environment

type Coord = (Int, Int)

parseCoord :: String -> Coord
parseCoord s = (read $ takeWhile (/= ',') s, read $ drop 1 $ dropWhile (/= ',') s)

inBounds :: (Int, Int) -> Coord -> Bool
inBounds (limitX, limitY) (a, b) = a >= 0 && b >= 0 && a <= limitX && b <= limitY

createGraphWithLimits :: (Int, Int) -> [Coord] -> Graph Coord Int
createGraphWithLimits l@(limitX, limitY) coords = Graph $ M.fromList [((x, y), map (`EdgeTo` 1) $ filter (inBounds l) $ filter (`notElem` coords) [(x, y + 1), (x + 1, y), (x - 1, y), (x, y - 1)]) | x <- [0 .. limitX], y <- [0 .. limitY]]

main :: IO ()
main = do
    [fileLoc] <- getArgs
    falling <- map parseCoord . lines <$> readFile fileLoc
    let graph = createGraphWithLimits (70, 70) $ take 1024 falling
        shortests = lightestPaths graph (0, 0) cumulativeWeighter
    -- Part 1
    print $ pathWeight $ pathsAsMap shortests M.! (70, 70)
    -- Part 2
    print $ (falling !!) . subtract 1 <$> find (\i -> M.notMember (70, 70) (pathsAsMap $ lightestPaths (createGraphWithLimits (70, 70) (take i falling)) (0, 0) cumulativeWeighter)) [1024 ..]
