import Data.List (sort)
import Data.Tuple.Extra (both)

main = interact $ \x -> show (one x, two x)

one = sum
    . map abs
    . uncurry (zipWith subtract)
    . both sort
    . pairs
    . map (read @Int)
    . words

two = sum
    -- n^2 time, let's go
    . (\(a, b) -> [x * length (filter (== x) b) | x <- a])
    . pairs
    . map (read @Int)
    . words

pairs :: [a] -> ([a], [a])
pairs [] = ([], [])
pairs (x:y:xs) = (x:xs', y:ys')
    where (xs', ys') = pairs xs