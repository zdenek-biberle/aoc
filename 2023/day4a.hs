import Data.List
f l=2^(length$l\\nub l)`div`2
main=interact$show.sum.map(f.words).lines