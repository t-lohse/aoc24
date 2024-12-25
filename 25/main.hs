import System.Environment

type Keynum = [Int]

defKeynum :: Keynum
defKeynum = [0, 0, 0, 0, 0]

fits :: Keynum -> Keynum -> Bool
fits key lock = all (\(k, l) -> k + l <= 5) $ zip key lock

parseSchematics :: [String] -> [[String]]
parseSchematics [] = []
parseSchematics xs = takeWhile (not . null) xs : parseSchematics (drop 1 $ dropWhile (not . null) xs)

schematicsToKeys :: [[String]] -> ([Keynum], [Keynum]) -- (Keys, Locks)
schematicsToKeys [] = ([], [])
schematicsToKeys (x@((x' : _) : _) : xs)
    | x' == '#' = (keys, searcher '#' (tail x) : locks)
    | x' == '.' = (searcher '#' (init x) : keys, locks)
  where
    (keys, locks) = schematicsToKeys xs

    searcher :: Char -> [String] -> Keynum
    searcher _ [] = defKeynum
    searcher c (x : xs) = zipWith (+) (map (\v -> if v == c then 1 else 0) x) $ searcher c xs

main :: IO ()
main = do
    [fileLoc] <- getArgs
    (keys, locks) <- schematicsToKeys . parseSchematics . lines <$> readFile fileLoc
    print keys
    print locks
    print $ length $ [(k, l) | k <- keys, l <- locks, fits k l]
