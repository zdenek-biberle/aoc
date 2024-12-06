import Control.Arrow
import Data.Graph
import Data.List.Extra
import Data.Map (Map)
import qualified Data.Map as Map


-- Challenge: Use something new.
-- I'm going to use Data.Graph. I've never used it before.
--
-- What have we learned? Not much, I didn't use Data.Graph quite enough.

main = interact $ show . (one &&& two) . parseIt

type Rules = Map Int [Int]
type Update = [Int]
type Input = (Rules, [Update])
parseIt :: String -> Input
parseIt s = (rules, updates)
    where
    [a, b] = splitOn "\n\n" s
    rulesList = map (\r -> let [n, m] = splitOn "|" r in (read n, read m)) $ lines a
    rules = Map.fromList $ fmap (fmap (fmap snd)) $ groupOnKey fst $ sort rulesList

    updates = (map read . splitOn ",") <$> lines b

one (rules, updates) = sum $ middle <$> filter isValidUpdate updates
    where
    isValidUpdate u = orderUpdate rules u == u
    
two (rules, updates) = sum $ middleOfFixOrZero <$> updates
    where
    middleOfFixOrZero u
        | ordered == u = 0
        | otherwise = middle ordered
        where ordered = orderUpdate rules u

pageToNode :: Rules -> Int -> (Int, Int, [Int])
pageToNode r p = (p, p, Map.findWithDefault [] p r)

orderUpdate :: Rules -> Update -> Update
orderUpdate r u = reverse $ flattenSCCs $ stronglyConnComp (pageToNode r <$> u)

middle :: [Int] -> Int
middle x = go x x
    where
    go (x:_) [_] = x
    go (_:xs) (_:_:ys) = go xs ys