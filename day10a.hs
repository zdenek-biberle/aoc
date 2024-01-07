import Data.Maybe
import qualified Data.Map.Strict as M

type Pipe = Char
type Pos = (Int, Int)
type Island = M.Map Pos Pipe

indexLines = concat . zipWith indexLine [1..]
indexLine y = zipWith (indexPipe y) [1..]
indexPipe y x p = ((x,y), p)

{-prune :: Island -> Island
prune m = let m' = M.filterWithKey (connects m) m in if m' == m then m else prune m'

connects :: Island -> Pos -> Pipe -> Bool
connects _ _ '.' = False
connects _ _ 'S' = True
connects m pos p = go p
    where
    pipe :: Dir -> (Pipe -> Bool) -> Bool
    pipe dir pred = case M.lookup (go dir pos) m of
        Nothing -> False
        Just p -> pred p
    go '|' = pipe N connectsSouth && pipe S connectsNorth
    go '-' = pipe W connectsEast && pipe E connectsWest
    go 'L' = pipe N connectsSouth && pipe E connectsWest
    go 'J' = pipe N connectsSouth && pipe W connectsEast
    go 'F' = pipe S connectsNorth && pipe E connectsWest
    go '7' = pipe S connectsNorth && pipe W connectsEast-}

data Dir = N | S | W | E deriving (Eq, Show)

go :: Dir -> Pos -> Pos
go N (x,y) = (x, y - 1)
go S (x,y) = (x, y + 1)
go W (x,y) = (x - 1, y)
go E (x,y) = (x + 1, y)

through' :: Pipe -> Dir -> Maybe Dir
through' '|' N = Just N
through' '|' S = Just S
through' '-' E = Just E
through' '-' W = Just W
through' 'L' S = Just E
through' 'L' W = Just N
through' 'J' S = Just W
through' 'J' E = Just N
through' 'F' N = Just E
through' 'F' W = Just S
through' '7' N = Just W
through' '7' E = Just S
through' _ _ = Nothing

through :: Pipe -> Dir -> Dir
through p d = fromMaybe (error $ concat ["Bruh you can't go " <> show d <> " into a " <> show p]) $ through' p d

connects S = (`elem` "|F7S")
connects N = (`elem` "|LJS")
connects W = (`elem` "-J7S")
connects E = (`elem` "-LFS")

f :: Island -> (Pos, Dir) -> (Pos, Dir) -> Int -> Int
f m (i1, _) (i2, _) acc | i1 == i2 = acc
f m a b acc = f m (move a) (move b) (succ acc)
    where 
    move (i, d) = let
            i' = go d i
            p' = m M.! i'
            d' = through p' d
        in (i', d')

findStart = head . M.keys . M.filter (=='S')

connections :: Island -> Pos -> [(Pos, Dir)]
connections m pos = mapMaybe eh [N,S,W,E]
    where
    eh d = do
        let pos' = go d pos
        p' <- M.lookup pos' m
        d' <- through' p' d
        pure (pos', d')

main = do
    m <- M.fromList . indexLines . lines <$> getContents
    let start = findStart m
    print start
    let [a, b] = connections m start
    print $ f m a b 1