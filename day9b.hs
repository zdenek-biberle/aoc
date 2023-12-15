f x|all(==0)x=0
f x=head x-f(zipWith(-)(tail x)x)
main=interact$show.sum.map(f.map read.words).lines