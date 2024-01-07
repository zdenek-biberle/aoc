import Control.Monad
import Data.Maybe
import qualified Data.Map.Strict as M
import qualified Data.Set as S

type Pipe = Char
type Pos = (Int, Int)
type Island = M.Map Pos Pipe

indexLines = concat . zipWith indexLine [1..]
indexLine y = zipWith (indexPipe y) [1..]
indexPipe y x p = ((x,y), p)

data Dir = N | S | W | E deriving (Eq, Show)

move :: Dir -> Pos -> Pos
move N (x,y) = (x, y - 1)
move S (x,y) = (x, y + 1)
move W (x,y) = (x - 1, y)
move E (x,y) = (x + 1, y)

rev :: Dir -> Dir
rev N = S
rev S = N
rev W = E
rev E = W

exits '|' = [N, S]
exits '-' = [E, W]
exits 'L' = [N, E]
exits 'J' = [N, W]
exits 'F' = [E, S]
exits '7' = [W, S]
exits 's' = [N, S, E, W]
exits _ = []

through' :: Pipe -> Dir -> Maybe Dir
through' p d | [o] <- filter (/= rev d) $ exits p = Just o
through' _ _ = Nothing

through :: Pipe -> Dir -> Dir
through p d = fromMaybe (error $ concat ["Bruh you can't go " <> show d <> " into a " <> show p]) $ through' p d

connects :: Dir -> Pipe -> Bool
connects _ 'S' = True
connects d p = d `elem` exits p

f :: Island -> (Pos, Dir) -> (Pos, Dir) -> [Pos] -> [Pos]
f m (i1, _) (i2, _) acc | i1 == i2 = i1 : acc
f m a b acc = f m (go a) (go b) (fst a : fst b : acc)
    where 
    go (i, d) = let
            i' = move d i
            p' = m M.! i'
            d' = through p' d
        in (i', d')

findStart = head . M.keys . M.filter (=='S')

connections :: Island -> Pos -> [(Pos, Dir)]
connections m pos = mapMaybe eh [N,S,W,E]
    where
    eh d = do
        let pos' = move d pos
        p' <- M.lookup pos' m
        d' <- through' p' d
        pure (pos', d')

findBounds m = (maximum . map fst $ M.keys m, maximum . map snd $ M.keys m)

printIsland m (w,h) =
    forM_ [1..h] $ \y ->
        putStrLn [M.findWithDefault ' ' (x, y) m | x <- [1..w]]

printFlood s (w,h) =
    forM_ [0..h] $ \y ->
        putStrLn [if (x,y) `S.member` s then '#' else ' ' | x <- [0..w]]

main = do
    m <- M.fromList . indexLines . lines <$> getContents
    let start = findStart m
    print start
    let bounds = findBounds m
    printIsland m bounds
    let [a, b] = connections m start
    let pipePositions = S.fromList $ f m a b [start]
    let m' = M.restrictKeys m pipePositions
    putStrLn "----"
    printIsland m' bounds
    let flooded = flood m' (S.singleton (70, 70))
    putStrLn "----"
    printFlood flooded bounds
    let floodedSpots = reduceFlood flooded
    putStrLn "----"
    printFlood floodedSpots bounds
    print $ S.size floodedSpots

{-

a tile at position (x,y) is surrounded by four points at positions:
(x-1, y-1), (x, y-1), (x-1, y), (x, y).

Like this:

     0 1 1 2 2 
  0  .   .   .
  1    F   -
  1  .   .   .
  2    L   -
  2  .   .   .

Now we need to flood the points so that we can figure out all the spots
surrounded by the loop. We simply try to expand each point in the four
directions, respecting whether a pipe is in the way
-}

flood :: Island -> S.Set Pos -> S.Set Pos
flood m s = if s == flooded then s else flood m flooded
    where
    flooded = s `S.union` (S.fromList $ concat $ map go $ S.elems s)
    go pos = [move dir pos | dir <- [N, S, W, E], canMove pos dir]
    canMove pos N = fromMaybe True $ not <$> connects E <$> M.lookup pos m
    canMove pos S = fromMaybe True $ not <$> connects E <$> M.lookup (move S pos) m
    canMove pos W = fromMaybe True $ not <$> connects S <$> M.lookup pos m
    canMove pos E = fromMaybe True $ not <$> connects S <$> M.lookup (move E pos) m

{- Now we turn the flooded points into flooded tiles -}
reduceFlood = collapse W . collapse N
    where collapse d s = S.filter (\p -> move d p `S.member` s) s