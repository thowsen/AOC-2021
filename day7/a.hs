import Data.List.Split (splitOn)
import Data.List (sort, foldl')

-- brutefest. better compile this one :D:DddDd:::D
main = do 
    inp <- sort . map read . splitOn "," . head . lines <$> readFile "input.txt"
    putStrLn $ "part A:\t" ++ show (f inp ((abs .) . (-)))
    putStrLn $ "part B:\t" ++ show (f inp (\x y -> sum $ take (abs (y-x)) [1..]))
  where 
    f inp h = minimum $ foldl' (\acc e -> g e inp h : acc) [] [1..maximum inp]
    g x inp h = sum (foldl' (\acc y -> (h x y) : acc ) [] inp)
 


