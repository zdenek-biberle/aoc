import Data.Char
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
        pure (from, (left, right))
    skipSpaces
    eof
    pure (path, graph)

walk (((path, graph), _):_) = go "AAA" (cycle path)
    where
    go "ZZZ" _ = 0
    go n (p:ps) = 1 + go (turn p (lookup n graph)) ps
    turn 'L' (Just (l, _)) = l
    turn 'R' (Just (_, r)) = r

main = interact $ show . walk . readP_to_S parser