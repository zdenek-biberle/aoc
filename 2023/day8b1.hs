{-# LANGUAGE BangPatterns #-}

import Debug.Trace
import Data.List
import Control.DeepSeq
import Data.Char
import qualified Data.Map.Strict as M
import Text.ParserCombinators.ReadP

str = munch1 isAlpha

parser = do
    path <- str
    skipSpaces
    graph <- flip sepBy1 (char '\n') $ do
        from <- str
        string " = ("
        left <- str
        string ", "
        right <- str
        string ")"
        pure (reverse from, (reverse left, reverse right))
    skipSpaces
    eof
    pure (path, M.fromList graph)

walk path graph = go [n | n <- M.keys graph, startNode n] (cycle path) 0
    where
    go :: [String] -> String -> Int ->  Int
    go ns _ !acc | ns `deepseq` all endNode ns = acc
    go ns (p:ps) !acc = go (map (turn p . (graph M.!)) ns) ps (acc + 1)
    turn 'L' (l, _) = l
    turn 'R' (_, r) = r

startNode (n:_) = n == 'A'
endNode (n:_) = n == 'Z'

main = interact $ show . uncurry walk . fst . head . readP_to_S parser