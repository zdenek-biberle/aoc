-- I'm too lazy to golf this

import Data.List
import Data.List.Split
import Data.Word

import Debug.Trace

f input = minimum $ map fst $ runMaps maps seeds
    where
    (rawSeeds:rawMaps) = splitOn "\n\n" input
    seeds = parseSeeds rawSeeds
    maps = map parseMap rawMaps

type Rule = (Word64, Word64, Word64)
type Map = [Rule]
type Range = (Word64, Word64)

parseMap :: String -> Map
parseMap = map (toRule . map read . words) . tail . lines
    where toRule [t, f, n] = (f, t, n)

parseSeeds :: String -> [Range]
parseSeeds = map toSeeds . chunksOf 2 . map read . tail . words
    where toSeeds [f, n] = (f, n)

runMap :: Map -> Range -> [Range]
runMap [] x = [x]
runMap (rule@(rf, rt, rn):rules) range@(sf, sn) =
    case splitSeeds rule range of
        [] -> runMap rules range
        ((sf', sn') : others) ->
            (sf' - rf + rt, sn') : runMap' rules others

-- If the rule affects the range then returns the range that fits within
-- the range of the rule and optionally any surrounding ranges
-- not affected by the rule. Crucially, this doesn't actually
-- map the range according to the rule.
splitSeeds :: Rule -> Range -> [Range]
splitSeeds (rf, _, rn) (sf, sn)
    | endsBefore = []
    | startsAfter = []

    -- map the whole range, easy
    | startsWithin, endsWithin =
        [(sf, sn)]

    -- cut into two pieces
    | startsBefore, endsWithin =
        [(rf, sn - (rf - sf)), (sf, rf - sf)]
    | startsWithin, endsAfter =
        [(sf, rn - (sf - rf)), (rf + rn, sf + sn - rf - rn)]

    -- full-on-Raiden
    | otherwise = [
        (rf, rn),
        (sf, rf - sf),
        (rf + rn, (sf + sn) - (rf + rn))
    ]

    where
    startsBefore = sf < rf
    startsWithin = not startsBefore && not startsAfter
    startsAfter = rf + rn <= sf
    endsBefore = sf + sn <= rf
    endsWithin = not endsBefore && not endsAfter
    endsAfter = rf + rn < sf + sn

runMap' :: Map -> [Range] -> [Range]
runMap' m = concatMap (runMap m)

runMaps :: [Map] -> [Range] -> [Range]
runMaps maps inputs = foldl (flip runMap') inputs maps

main = interact $ show . f