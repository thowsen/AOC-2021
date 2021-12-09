import Data.List (foldl', nub, sort)
import Data.List.Split (splitOn)
import qualified Data.Map as M

main = do
  inp <- map read . splitOn "," <$> readFile "input.txt"
  let solver = \x -> map snd $ foldl' (\acc _ -> tickGroups acc) (groupFishes inp) [1 .. x]
  putStrLn $ "part A\t" ++ show (sum $ solver 80)
  putStrLn $ "part B\t" ++ show (sum $ solver 256)

groupFishes :: [Int] -> [(Int, Int)]
groupFishes xs = [(x, length (filter (== x) xs)) | x <- nub $ sort xs]

tickGroups :: [(Int, Int)] -> [(Int, Int)]
tickGroups = M.toList . merge . foldl' (\acc (d, num) -> f d num acc) []
  where
    f d num acc = if d -1 < 0 then (6, num) : (8, num) : acc else (d -1, num) : acc
    merge xs = foldl' (\acc (k, v) -> M.insertWith (+) k v acc) M.empty xs