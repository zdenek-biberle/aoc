-- I'm too lazy to golf this

import Data.List
import Data.List.Split
import Data.Word

f input = minimum $ runMaps maps seeds
    where
    (rawSeeds:rawMaps) = splitOn "\n\n" input
    seeds = map read . tail . words $ rawSeeds :: [Word64]
    maps = map parseMap rawMaps

type Map = [(Word64, Word64, Word64)]

parseMap :: String -> Map
parseMap = map (toRule . map read . words) . tail . lines
    where toRule [t, f, n] = (f, t, n)

runMap :: Map -> Word64 -> Word64
runMap [] x = x
runMap ((from, to, count):rules) x
    | x >= from, x < from + count = x - from + to
    | otherwise = runMap rules x

runMap' :: Map -> [Word64] -> [Word64]
runMap' m = map (runMap m)

runMaps :: [Map] -> [Word64] -> [Word64]
runMaps maps inputs = foldl (flip runMap') inputs maps

main = do
    interact $ show . f