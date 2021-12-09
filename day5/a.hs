import Control.Arrow (second)
import Data.List (foldl')
import Data.List.Split (splitOn)
import qualified Data.Map as M

data Vector = Vector (Int, Int) (Int, Int) deriving (Show, Eq)

main = do
  inp <- fold . concatMap (map ((\[x, y] -> (read x, read y)) . words . map c2p) . splitOn "->") . lines <$> readFile "input.txt"
  let x = foldl' (flip drawVector) M.empty inp
  putStrLn $ "part B:\t" ++ show (foldl (\acc e -> if e > 1 then acc + 1 else acc) 0 x)
  where
    c2p x = if x == ',' then ' ' else x
    fold [] = [] -- cba to fix this :/
    fold (x : y : zs) = Vector x y : fold zs

drawVector :: Vector -> M.Map (Int, Int) Int -> M.Map (Int, Int) Int
drawVector v@(Vector (x1, y1) (x2, y2)) space
  | x1 > x2 && y1 > y2 = i space (take (x1 - x2 + 1) $ zip [x2 ..] [y2 ..]) -- comment out for part A lul
  | x1 > x2 && y1 < y2 = i space (take (x1 - x2 + 1) $ zip [x2 ..] [y2, y2 -1 ..]) -- comment out for part A lul
  | x1 >= x2 && y1 == y2 = i space (take (x1 - x2 + 1) $ zip [x2 ..] [y2, y2 ..])
  | x1 == x2 && y1 >= y2 = i space (take (y1 - y2 + 1) $ zip [x1, x1 ..] [y2 ..])
  | otherwise = drawVector (Vector (x2, y2) (x1, y1)) space -- fliperino! plis no infinite recursion...
  where
    i = foldl (\m p -> M.insertWith (+) p 1 m)