import Control.Arrow
import Data.Char

main = interact $ show . (one &&& two) . parseIt

parseIt :: String -> [(Int, [Int])]
parseIt s = (parseLine . words) <$> lines s
    where
    parseLine (total:operands) = (read $ takeWhile isDigit total, read <$> operands)

-- yeah yeah, I could be smarter by doing
-- x * 10 ^ (1 + floor $ logBase 10 y) + y
-- but who has time for that? in today's floating
-- point economy?
concatenate :: Int -> Int -> Int
concatenate x y = read $ show x ++ show y

solve :: [(Int -> Int -> Int)] -> [(Int, [Int])] -> Int
solve ops = sum . map fst . filter isGood
    where
    isGood (total, operands) = any (== total) $ results operands
    results :: [Int] -> [Int]
    results [x] = [x]
    results (x:y:ys) = do
        op <- ops
        results $ (op x y):ys

one = solve [(+), (*)]
two = solve [(+), (*), concatenate]
