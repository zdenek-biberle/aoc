import Control.Arrow
import Control.Monad
import Data.List
import qualified Data.Set as S
import Data.Set (Set)

main = interact $ show . (one &&& two) . parseIt

type XY = (Int, Int)
type YX = (Int, Int)
type Dir = Char
data Input = MkInput {
    _obstacles :: Set XY,
    _start :: XY,
    _startDir :: Dir,
    _size :: XY
} deriving Show

data Room = MkRoom {
    _obstaclesV :: Set XY,
    _obstaclesH :: Set YX
} deriving Show

move :: XY -> Dir -> XY
move (x, y) '^' = (x, y - 1)
move (x, y) 'v' = (x, y + 1)
move (x, y) '<' = (x - 1, y)
move (x, y) '>' = (x + 1, y)

turnRight :: Dir -> Dir
turnRight '^' = '>'
turnRight '>' = 'v'
turnRight 'v' = '<'
turnRight '<' = '^'

parseIt :: String -> Input
parseIt s = MkInput obstacles (startX, startY) startDir (width, height)
    where
    inLines = lines s
    positions = [ (x, y, c) | (y, l) <- zip [0..] inLines, (x, c) <- zip [0..] l ]
    obstacles = S.fromList $ [ (x, y) | (x, y, c) <- positions, c == '#' ]
    [(startX, startY, startDir)] = [ (x, y, c) | (x, y, c) <- positions, c `elem` "^v<>"]
    height = length $ inLines
    width = length $ head $ inLines

obstaclesToRoom :: Set XY -> Room
obstaclesToRoom obstacles = MkRoom obstacles (S.map (\(x, y) -> (y, x)) obstacles)

nextTurn :: XY -> Dir -> Room -> Maybe XY
nextTurn (x, y) '>' room | Just (y', x') <- S.lookupGT (y, x) (_obstaclesH room), y' == y = Just (x' - 1, y')
nextTurn (x, y) '<' room | Just (y', x') <- S.lookupLT (y, x) (_obstaclesH room), y' == y = Just (x' + 1, y')
nextTurn (x, y) 'v' room | Just (x', y') <- S.lookupGT (x, y) (_obstaclesV room), x' == x = Just (x', y' - 1)
nextTurn (x, y) '^' room | Just (x', y') <- S.lookupLT (x, y) (_obstaclesV room), x' == x = Just (x', y' + 1)
nextTurn _ _ _ = Nothing

toEdge :: XY -> XY -> Dir -> XY
toEdge (w, h) (x, y) '^' = (x, 0)
toEdge (w, h) (x, y) 'v' = (x, h - 1)
toEdge (w, h) (x, y) '<' = (0, y)
toEdge (w, h) (x, y) '>' = (w - 1, y)

positionsBetween :: XY -> XY -> [XY]
positionsBetween (x1, y1) (x2, y2) | x1 == x2 = [ (x1, y) | y <- [min y1 y2..max y1 y2] ]
positionsBetween (x1, y1) (x2, y2) | y1 == y2 = [ (x, y1) | x <- [min x1 x2..max x1 x2] ]

one i = length $ nub $ go (_start i) (_startDir i)
    where
    go pos dir | Just pos' <- nextTurn pos dir room = positionsBetween pos pos' <> go pos' (turnRight dir)
    go pos dir = positionsBetween pos (toEdge (_size i) pos dir)
    room = obstaclesToRoom $ _obstacles i

loops :: Input -> Bool
loops i = go (_start i) (_startDir i) S.empty
    where
    go pos dir visited | (pos, dir) `S.member` visited = True
    go pos dir visited | Just pos' <- nextTurn pos dir room =
        go pos' (turnRight dir) (S.insert (pos, dir) visited)
    go _ _ _ = False
    room = obstaclesToRoom $ _obstacles i

two i = length $ do
    let (width, height) = _size i
    let allPositions = S.fromList [ (x, y) | x <- [0..width - 1], y <- [0..height - 1] ]
    let freePositions = allPositions S.\\ (_obstacles i)
    (x, y) <- S.toList freePositions
    guard $ loops i { _obstacles = S.insert (x, y) (_obstacles i) }
    pure (x, y)