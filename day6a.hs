f input = show $ product $ zipWith solve times records
    where
    [(_:times), (_:records)] = map (map read . words) $ lines input

-- x is the time spent holding the button
-- -x^2 + t * x - r = 0
-- so we just use the quadratic formula
solve t r = 1 + floor (go (-)) - ceiling (go (+))
    where go (!) = ((-t) ! sqrt (t*t - 4*r)) / (-2)

main = interact f
