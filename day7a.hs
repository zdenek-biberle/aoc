import Data.List
f[5]=7
f[1,_]=6
f[2,3]=5
f[1,1,3]=4
f[1,2,2]=3
f[1,1,1,2]=2
f _=1
q 'A'='X'
q 'K'='W'
q 'Q'='V'
q 'J'='U'
q a=a
g h=(f$sort$map length$group$sort h,q<$>h)
w[a,b]=(a,read b)
main=interact$show.sum.zipWith((.snd).(*))[1..].sortOn(g.fst).map(w.words).lines