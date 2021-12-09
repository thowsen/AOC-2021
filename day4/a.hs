import Control.Arrow (second)
import Data.List (foldl', transpose)
import Data.List.Split (chunksOf, splitOn)
import Data.Maybe (fromMaybe)

data Marked = Crossed Int | Uncrossed Int deriving (Show, Eq)

type Tile = [[Marked]]

main = do
  (x : xs) <- filter (/= "") . lines <$> readFile "input.txt"
  let draws = map (read :: String -> Int) $ splitOn "," x
  let bingoTiles = map (map (map (Uncrossed . read) . words)) $ chunksOf 5 xs
  putStrLn $ "part A: " ++ show (countPoints $ play draws bingoTiles)
  putStrLn $ "part B: " ++ show (countPoints $ play2 draws bingoTiles)

--play :: [Int] -> [Tile] -> Tile
play :: Foldable t => t Int -> [Tile] -> (Int, Tile)
play = (second (head . filter hasBingo) .) . fld
  where
    fld xs ys = foldl' (\o@(l, acc) e -> if any hasBingo acc then o else (e, playTiles e acc)) (0, ys) xs

play2 :: [Int] -> [Tile] -> (Int, Tile)
play2 xs ts = fromMaybe (0, []) $ f xs (zip (repeat False) ts) [] Nothing
  where
    f :: [Int] -> [(Bool, Tile)] -> [(Bool, Tile)] -> Maybe (Int, Tile) -> Maybe (Int, Tile)
    f [] _ _ t = t
    f (i : is) [] ts t = f is ts [] t
    f ys@(y : _) (x : xs) ts' t = case x of
      (True, c) -> f ys xs ((True, playTile y c) : ts') t
      (_, c) ->
        let hb = hasBingo (playTile y c)
            pt = playTile y c
         in f ys xs (if hb then (True, pt) : ts' else (False, pt) : ts') (if hb then Just (y, pt) else t)

countPoints :: (Int, Tile) -> Int
countPoints (i, t) = i * sum (map (sum . map f) t)
  where
    f (Uncrossed p) = p
    f _ = 0

playTiles :: Int -> [Tile] -> [Tile]
playTiles _ [] = []
playTiles n (x : xs) = playTile n x : playTiles n xs

playTile :: Int -> Tile -> Tile
playTile n = map (map (\x -> if x == Uncrossed n then Crossed n else x))

hasBingo :: Tile -> Bool
hasBingo xs = any (all isCrossed) xs || any (all isCrossed) (transpose xs)
  where
    isCrossed (Crossed _) = True
    isCrossed _ = False