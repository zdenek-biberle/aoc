{-# LANGUAGE BangPatterns #-}

import Debug.Trace
import Control.DeepSeq
import Data.List
import Data.Char
import qualified Data.Map.Strict as M
import Data.Word
import Text.ParserCombinators.ReadP

str = munch1 isAlpha

type Node = Int

strToNode :: String -> Int
strToNode = sum . zipWith (\e c -> (26^e) * (ord c - ord 'A')) [0..]

node = strToNode <$> str

parser = do
    path <- str
    skipSpaces
    graph <- flip sepBy1 (char '\n') $ do
        from <- node
        string " = ("
        left <- node
        string ", "
        right <- node
        string ")"
        pure (from, (left, right))
    skipSpaces
    eof
    pure (path, M.fromList graph)

walk path graph = go [n | n <- M.keys graph, startNode n] (cycle path) 0
    where
    go :: [Node] -> String -> Word64 -> Word64
    go ns _ !acc | ns `deepseq` all endNode ns = acc
    go ns (p:ps) !acc = go (map (turn p . (graph M.!)) ns) ps (acc + 1)
    turn 'L' (l, _) = l
    turn 'R' (_, r) = r

aaa = strToNode "AAA"
zza = strToNode "ZZA"
aaz = strToNode "AAZ"
zzz = strToNode "ZZZ"

startNode :: Node -> Bool
startNode n = aaa <= n && n <= zza
endNode :: Node -> Bool
endNode n = aaz <= n && n <= zzz

main = interact $ show . uncurry walk . fst . head . readP_to_S parser