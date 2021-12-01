module Main where

main = solveB . map read . words <$> readFile "inp.txt"

solveB = solveA . format
    where format [_,_] = []
          format xs = sum (take 3 xs) : format (tail xs)

solveA :: [Int] -> Int
solveA xs = length $ filter (uncurry (>)) (zip (tail xs) xs)