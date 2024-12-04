{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE StandaloneDeriving #-}

import Control.Applicative
import Control.Arrow
import Data.Foldable1 as F1
import Data.List
import Data.List.NonEmpty (NonEmpty(..))
import Data.Maybe
import Text.Show.Functions

-- Challenge: Write my own regex engine.
--
-- What have we learned?
-- I don't think that this is a regex engine. It's more like parser combinators.

data Regex c a where
    REmpty :: Regex c ()
    RFail :: Regex c a
    RLiteral :: c -> Regex c c
    RSeq :: Regex c a -> Regex c b -> Regex c (a, b)
    RAlt :: Regex c a -> Regex c b -> Regex c (Either a b)
    RStar :: Regex c a -> Regex c [a]
    RMap :: (a -> b) -> Regex c a -> Regex c b

deriving instance (Show c) => Show (Regex c a)

instance Functor (Regex c) where
    fmap = RMap

oneOfR :: [Regex c a] -> Regex c a
oneOfR [] = RFail
oneOfR (r:rs) = either id id <$> RAlt r (oneOfR rs)

oneOfR' :: [c] -> Regex c c
oneOfR' cs = oneOfR (RLiteral <$> cs)

seqR :: [Regex c a] -> Regex c [a]
seqR [] = const [] <$> REmpty
seqR (r:rs) = uncurry (:) <$> RSeq r (seqR rs)

seqR' :: [c] -> Regex c [c]
seqR' cs = seqR (RLiteral <$> cs)

seqRFst :: Regex c a -> Regex c b -> Regex c a
seqRFst r1 r2 = fst <$> RSeq r1 r2

seqRSnd :: Regex c a -> Regex c b -> Regex c b
seqRSnd r1 r2 = snd <$> RSeq r1 r2

plusR :: Regex c a -> Regex c (NonEmpty a)
plusR r = uncurry (:|) <$> RSeq r (RStar r)

(<$$>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<$$>) = fmap . fmap

parse :: Eq c => Regex c a -> [c] -> Maybe ([c], a)
parse REmpty cs = Just (cs, ())
parse RFail _ = Nothing
parse (RLiteral c) (c':cs) | c == c' = Just (cs, c)
parse (RLiteral _) _ = Nothing
parse (RSeq r1 r2) cs = do
    (cs', a) <- parse r1 cs
    (cs'', b) <- parse r2 cs'
    pure (cs'', (a, b))
parse (RAlt r1 r2) cs =
    (Left <$$> parse r1 cs) <|> (Right <$$> parse r2 cs)
parse (RStar r) cs = Just (go cs)
    where 
    go cs = case parse r cs of
        Just (cs', a) -> let (cs'', as) = go cs' in (cs'', a:as)
        Nothing -> (cs, [])
parse (RMap f r) cs = f <$$> parse r cs

parseAll :: Eq c => Regex c a -> [c] -> [a]
parseAll r = mapMaybe ((snd <$>) . parse r) . tails

-- generic parser stuff ends here and the solution starts

digitR :: Regex Char Int
digitR = oneOfR [
    0 <$ RLiteral '0',
    1 <$ RLiteral '1',
    2 <$ RLiteral '2',
    3 <$ RLiteral '3',
    4 <$ RLiteral '4',
    5 <$ RLiteral '5',
    6 <$ RLiteral '6',
    7 <$ RLiteral '7',
    8 <$ RLiteral '8',
    9 <$ RLiteral '9']

numberR :: Regex Char Int
numberR = F1.foldl1 (\a b -> a * 10 + b) <$> plusR digitR

mulR :: Regex Char (Int, Int)
mulR = seqR' "mul("
    `seqRSnd` numberR
    `seqRFst` (RLiteral ',')
    `RSeq` numberR
    `seqRFst` (RLiteral ')')

doR = seqR' "do()"

dontR = seqR' "don't()"

data Instruction = Do | Dont | Mul Int Int deriving Show

instructionR :: Regex Char Instruction
instructionR = oneOfR [uncurry Mul <$> mulR, Do <$ doR , Dont <$ dontR]

main = interact $ show . (one &&& two)

one = sum . fmap evalMuls . parseAll mulR
two = sum . snd . mapAccumL eval True . parseAll instructionR
    where
    eval _ Do = (True, 0)
    eval _ Dont = (False, 0)
    eval True (Mul a b) = (True, a * b)
    eval False _ = (False, 0)

evalMuls :: (Int, Int) -> Int
evalMuls (a, b) = a * b