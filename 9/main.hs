import Control.Applicative
import Data.Bifunctor qualified as BF
import Data.Char
import System.Environment

data Block a
    = Data
        { data_ :: a
        , count :: Int
        , nxt_ :: Block a
        }
    | Free Int (Block a)
    | End

instance (Show a) => Show (Block a) where
    show End = ""
    show (Data a i n) = concat (replicate i ("(" ++ show a ++ ")")) ++ show n
    show (Free i n) = replicate i '.' ++ show n

combine :: Block a -> Block a -> Block a
combine End v = v
combine (Data a i End) v = Data a i v
combine (Free i End) v = Free i v
combine (Data a i x) v = Data a i $ x `combine` v
combine (Free i x) v = Free i $ x `combine` v

parsedStr :: String -> Int -> Block Int
parsedStr (x : y : xs) i = Data i (digitToInt x) End `combine` Free (digitToInt y) End `combine` parsedStr xs (i + 1)
parsedStr [a] i = Data i (digitToInt a) End
parsedStr [] _ = End

takeLast :: Block a -> (a, Block a)
takeLast (Data a 1 End) = (a, End)
takeLast (Data a i End) = (a, Data a (i - 1) End)
takeLast (Data a 1 (Free _ End)) = (a, End)
takeLast (Data a i (Free _ End)) = (a, Data a (i - 1) End)
takeLast (Data a i n) = let (a', n') = takeLast n in (a', Data a i n')
takeLast (Free i n) = let (a', n') = takeLast n in (a', Free i n')
takeLast End = error "oof"

takeLastSized :: Block a -> Int -> Maybe ([a], Block a)
takeLastSized (Data a i End) c
    | i <= c = return (replicate i a, End)
    | otherwise = Nothing
takeLastSized (Data a i n) c
    | i <= c = BF.second (Data a i) <$> takeLastSized n c <|> return (replicate i a, Free i n)
    | otherwise = BF.second (Data a i) <$> takeLastSized n c
takeLastSized (Free i n) c = BF.second (Free i) <$> takeLastSized n c
takeLastSized End _ = Nothing

compact :: Block Int -> [Int]
compact End = []
compact (Data a i n) = replicate i a ++ compact n
compact (Free i n) = v ++ compact n'
  where
    (v, n') = foldl (\(vl, nl) _ -> let (vl', nl') = takeLast nl in (vl ++ [vl'], nl')) ([], n) [1 .. i]

compactBlocks :: Block Int -> [Int]
compactBlocks End = []
compactBlocks (Data a i n) = replicate i a ++ compactBlocks n
compactBlocks (Free i n) = case takeLastSized n i of
    Just (v, n') -> v ++ if l == i then compactBlocks n' else compactBlocks $ Free (i - l) n'
      where
        l = length v
    Nothing -> replicate i 0 ++ compactBlocks n

main :: IO ()
main = do
    [fileloc] <- getArgs
    [file] <- lines <$> readFile fileloc
    let parsed = parsedStr file 0
    -- Part 1
    print $ sum $ zipWith (*) (compact parsed) [0 ..]
    -- Part 2
    print $ sum $ zipWith (*) (compactBlocks parsed) [0 ..]
