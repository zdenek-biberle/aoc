import Control.Applicative
import Control.Monad
import Data.List

-- Challenge: Whenever reasonably possible, write things in point-free style.
--
-- What have we learned?
-- That `ap (f . g) h` ≡ `liftA2 f g h` for functions. The `ap` version is not
-- terribly readable, IMO.

main = interact $ show
    . liftA2 (,) (solve undampened) (solve dampened)
    . map (map read . words)
    . lines

solve damping = length . filter (any asc . damping)

asc :: [Int] -> Bool
asc = all uhh . ap zip tail

-- Idk what to call this one
uhh :: (Int, Int) -> Bool
uhh (a, b) = a < b && b - a <= 3

undampened :: [Int] -> [[Int]]
undampened = (ap (:) (pure . reverse))

dampened :: [Int] -> [[Int]]
dampened = (undampened =<<)
    . liftA2 (zipWith (<>)) inits (map (drop 1) . tails)