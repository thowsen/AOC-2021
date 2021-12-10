import Data.List.Split (splitOn)
import Data.List (nub, foldl', sort, intersect)
import Debug.Trace (trace)
import qualified Data.Map as M

data Entry = Entry {pattern :: [String], output:: [String] } deriving (Show, Eq, Ord)

-- Please dont judge me... no bruteforce tho!

main = do 
  inp <- map ((\[a,b] -> Entry a b) . map (map sort . words) .splitOn "|") . lines <$> readFile "input.txt"
  putStrLn $ "part A:\t" ++ show (length $ filter (\x -> length x `elem` [2,4,3,7])  $ concat $ map (output) inp)
  putStrLn $ "part B:\t" ++ show (sum (solve_b inp))

solve_b :: [Entry] -> [Int]
solve_b [] = []
solve_b (x:xs) = do 
  let m = M.fromList $ map (\(x,y) -> (y,x)) $ M.toList $  solve_pattern (pattern x)
  let o = read $ concatMap (\x -> show $ find' x m) (output x)
  o : solve_b xs

solve_pattern :: [String] -> M.Map Int String
solve_pattern xs = init `M.union` (M.fromList [(5,five),(9,nine),(3,three),(6,six),(0,zero),(2,two)])
  where 
    init = solve_unique xs M.empty
    candidates = filter (\x -> x `notElem` (map snd $ M.toList init)) xs
    d = (\x -> trace ("d is: " ++ show x) x) $ uncurry difference (find 7 init, find 1 init)
    g = head $ filter (\x -> length x == 1) $ map (\x -> difference x (merge d (find 4 init))) xs
    five = do 
      let four = find 4 init 
      let one = find 1 init 
      head $ flip filter candidates $ \x -> length (difference x (difference four one)) == 3 
    nine = merge five (find 1 init)
    e = difference (find 8 init) nine
    three = head $ filter (\x -> (head e `notElem` x) && length x == 5) $ difference candidates [five,nine]
    six = head $ filter (\x -> merge e five == x) xs 
    zero = head $ filter (\x -> length x == 6 && x `elem` (difference candidates [nine, three, six, five])) xs
    two = head $ filter (\x -> x `elem` (difference candidates [nine,three,six,five,zero])) xs

solve_unique :: [String] -> M.Map Int [Char] -> M.Map Int [Char]
solve_unique x m =  foldl' (\acc e -> f e acc) m x 
  where
    uniques = [(2,1),(4,4),(3,7),(7,8)] 
    f x m
      | length x == 2 = g 1 x
      | length x == 4 = g 4 x
      | length x == 3 = g 7 x
      | length x == 7 = g 8 x
      | otherwise = m
     where g num segments = M.insert num segments m

find = M.findWithDefault "fail"
find' = M.findWithDefault (-1)
merge x y = sort $ nub $ x ++ y
difference x y = [x' | x' <- x, x' `notElem` y]