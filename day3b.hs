import Data.List
s c=c>'/'&&c<=':'
f[]=[]
f(d:e)|s d=j$span s$d:e|1>0=[]:f e
j(a,b)=(a*>[[read a]])<>f b
g[]=[]
g(a:[]:c:d)=a:(a<>c):g(c:d)
g([]:b:c)=b:g(b:c)
g[a,[]]=[a,a]
g(a:b)=a:g b
q=(sum.).zipWith(t.concat).transpose.take 3
t[a,b]'*'=a*b
t _ _=0
h i=show$sum$zipWith q(tails$repeat[]:map(g.f)i)i
main=interact$h.lines