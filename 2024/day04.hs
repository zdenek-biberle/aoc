import Data.List
import Control.Applicative
import Control.Arrow

-- Chalenge: Don't do any indexing.

main = interact $ show . (one &&& two) . lines
one = sum . map (sum . map countXmasOnLine) . variants
two l = sum $ zipWith3 countX l (tail l) (tail $ tail l)

stagger :: [String] -> [String]
stagger = zipWith (<>) [replicate n '.' | n <- [0..]]

variants :: [String] -> [[String]]
variants xs = [xs, transpose xs, transpose $ stagger xs, transpose $ stagger $ reverse xs]

countXmasOnLine :: String -> Int
countXmasOnLine = length . filter (liftA2 (||) (isPrefixOf "XMAS") (isPrefixOf "SAMX")) . tails

-- This looks goofy af
isX :: String -> String -> String -> Int
isX ('M':_:'M':_) (_:'A':_) ('S':_:'S':_) = 1
isX ('M':_:'S':_) (_:'A':_) ('M':_:'S':_) = 1
isX ('S':_:'M':_) (_:'A':_) ('S':_:'M':_) = 1
isX ('S':_:'S':_) (_:'A':_) ('M':_:'M':_) = 1
isX _ _ _ = 0

countX :: String -> String -> String -> Int
countX a b c = sum $ zipWith3 isX (tails a) (tails b) (tails c)
