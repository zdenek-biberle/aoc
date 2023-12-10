import Data.List
f[]=7
f[_]=7
f[1,_]=6
f[2,_]=5
f[1,1,_]=4
f[1,2,2]=3
f[1,1,1,_]=2
f _=1
q 'A'='X'
q 'K'='W'
q 'Q'='V'
q 'J'='1'
q a=a
g h=(f$sort$map length$group$sort$filter(/='J')h,q<$>h)
w[a,b]=(a,read b)
main=interact$show.sum.zipWith((.snd).(*))[1..].sortOn(g.fst).map(w.words).lines