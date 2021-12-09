import Data.Char (digitToInt)
import Data.List (foldl')

-- HORRIBLE CODE BELOW.

main = do
  inp <- lines <$> readFile "input.txt"
  let len = sum $ map (uncurry (*)) $ toNum $ replicate (length $ head inp) 1
  putStrLn $ "part A:\t" ++ show ((\x -> (len - x) * x) $ (sum . map (uncurry (*)) . toNum . solveA) inp)
  let toBinMatrix = \f -> flip g f $ map (map ((fromIntegral :: Int -> Integer) . digitToInt)) inp
  putStrLn $ "part B:\t" ++ show (product $ map (sum . map (uncurry (*)) . toNum . toBinMatrix) [(>=), (<)])

solveA :: [String] -> [Integer]
solveA xs =
  map
    (\x -> if (fromIntegral x / fromIntegral (length xs)) > 0.5 then 1 else 0)
    (a (replicate (length xs) 0) xs)
  where
    a = foldl' (flip f)
    f = zipWith (\x y -> (if x > '0' then y + 1 else y))

--toNum :: [Integer] -> [Integer]
toNum xs = zip (reverse (map (2 ^) [0 .. (length xs - 1)])) xs

mostCommon :: (Fractional a) => [[Integer]] -> (a -> a -> Bool) -> Integer
mostCommon xs f =
  if fromIntegral (length $ filter (> 0) $ map head xs) `f` (fromIntegral (length xs) / 2)
    then 1
    else 0

g :: (Fractional a) => [[Integer]] -> (a -> a -> Bool) -> [Integer]
g [] _ = []
g [x] _ = x -- fk these patternmatches...
g ([] : _) _ = []
g xs f = do
  let dominant = mostCommon xs f
  let l' = filter ((== dominant) . head) xs
  dominant : g (map tail l') f
