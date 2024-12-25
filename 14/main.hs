import Data.List
import System.Environment

type Coord = (Int, Int)
type Velocity = (Int, Int)
type Robot = (Coord, Velocity)

parseRobot :: String -> Robot
parseRobot s = (p', v')
  where
    [p, v] = map (drop 2) $ words s
    sep v x = (takeWhile (/= v) x, drop 1 $ dropWhile (/= v) x)
    p' = let (x, y) = sep ',' p in (read x, read y)
    v' = let (x, y) = sep ',' v in (read x, read y)

scaleRobot :: Int -> (Int, Int) -> Robot -> Robot
scaleRobot scale (xLimit, yLimit) ((px, py), v@(vx, vy)) = new
  where
    (vx', vy') = (vx * scale, vy * scale)
    (px', py') = (px + vx', py + vy')
    new = ((px' `mod` xLimit, py' `mod` yLimit), v)

quadrants :: (Int, Int) -> [Robot] -> [[Robot]]
quadrants (xLimit, yLimit) xs = [topLeft, topRight, botLeft, botRight]
  where
    midV = xLimit `div` 2
    midH = yLimit `div` 2
    topLeft = filter (\((x, y), _) -> x < midV && y < midH) xs
    topRight = filter (\((x, y), _) -> x > midV && y < midH) xs
    botLeft = filter (\((x, y), _) -> x < midV && y > midH) xs
    botRight = filter (\((x, y), _) -> x > midV && y > midH) xs

main :: IO ()
main = do
    [fileloc] <- getArgs
    file <- lines <$> readFile fileloc
    let robots = map parseRobot file
        cap = (101, 103)
    -- Part 1
    print $ product $ map length $ quadrants cap $ map (scaleRobot 100 cap) robots
    -- Part 2 (Evil solution)
    print $ snd $ maximumBy ((pure (compare) <*> maximum <*> minimum) . fst) $ map (\i -> (map length $ quadrants cap $ map (scaleRobot i cap) robots, i)) [0 .. 10000]
