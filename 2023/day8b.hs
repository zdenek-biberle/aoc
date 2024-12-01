{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TupleSections #-}

import Control.Monad
import Data.Char
import Data.List
import Data.Maybe
import Data.Word
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Text.ParserCombinators.ReadP

str = munch1 isAlpha

parser = do
    path <- str
    skipSpaces
    nodes <- flip sepBy1 (char '\n') $ do
        from <- str
        string " = ("
        left <- str
        string ", "
        right <- str
        string ")"
        pure (reverse from, (reverse left, reverse right))
    skipSpaces
    eof
    pure (path, nodes)

type NodeId = String
data Node = Node { nodeId :: NodeId, isStartNode :: Bool, isEndNode :: Bool, leftNode :: Node, rightNode :: Node }
type Graph = M.Map String Node
type Step = (Int, Char)
type Path = [Step]
type EndMap = M.Map (NodeId, Step) (Word64, Node, Step)
data Ghost = G { gWalked :: !Word64, gStep :: !Step, gNode :: !Node }
    deriving Show

instance Eq Node where
    a == b = nodeId a == nodeId b

instance Show Node where
    show n = nodeId n

buildGraph :: [(String, (String, String))] -> Graph
buildGraph nodes = graph
    where
    graph = M.mapWithKey buildNode $ M.fromList nodes
    buildNode k (l,r) = Node k (startNode k) (endNode k) (graph M.! l) (graph M.! r)

findEnds :: Path -> Node -> [(Step, Word64, Node, Step)] 
findEnds path node = catMaybes $ zipWith walkToEnd path (tails path)
    where
    walkToEnd startStep ps = go node (ps <> cycle path) 0 S.empty
        where
        go n (p:_) !acc !vis | isEndNode n, acc > 0 = Just (startStep, acc, n, p)
        go n (p:ps) !acc !vis | S.member (nodeId n, p) vis = Nothing
        go n (p:ps) !acc !vis = go (turn p n) ps (succ acc) (S.insert (nodeId n, p) vis)

buildEndMap :: Path -> Graph -> EndMap
buildEndMap path = M.fromList . concatMap go . filter isEndNode . M.elems
    where
    go n = (\(s, d, e, s') -> ((nodeId n, s), (d, e, s'))) <$> findEnds path n

walkGhosts :: Path -> Graph -> EndMap -> [Ghost]
walkGhosts path graph endMap = go (map (walkToFirstEnd (cycle path) 0) $ filter isStartNode $ M.elems graph)
    where
    go !gs | allSame gWalked gs = gs
    go !gs = go $ map (walkTo (maximum $ gWalked <$> gs)) gs
    walkTo w g@G { gWalked = walked } | walked >= w = g
    walkTo w g = case endMap M.! (nodeId $ gNode g, gStep g) of
        (d, n, s) -> walkTo w $ G (gWalked g + d) s n
    walkToFirstEnd (p:_) !acc n | isEndNode n = G acc p n
    walkToFirstEnd (p:ps) !acc n = walkToFirstEnd ps (succ acc) (turn p n)

allSame _ [] = True
allSame f (x:xs) = let fx = f x in all ((==fx) . f) xs
startNode (n:_) = n == 'A'
endNode (n:_) = n == 'Z'

class IsStep a where
    turn :: a -> Node -> Node

instance IsStep Char where
    turn 'L' (Node { leftNode = l }) = l
    turn 'R' (Node { rightNode = r }) = r

instance IsStep b => IsStep (a, b) where
    turn (_, b) = turn b

main = do
    c <- getContents
    let (rawPath, rawGraph) = fst . head . readP_to_S parser $ c
    let path = zip [1..] rawPath
    let graph = buildGraph rawGraph
    let endMap = buildEndMap path graph
    putStrLn $ concat ["Got ", show $ M.size endMap, " links in endMap"]
    print endMap
    print $ walkGhosts path graph endMap