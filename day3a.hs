import Data.List
s c=c>'/'&&c<=':'
f[]=[]
f(d:e)|s d=j$span s$d:e|1>0=0:f e
j(a,b)=(a*>[read a])<>f b
g[]=[]
g(a:0:c:d)=a:(a+c):g(c:d)
g(0:b:c)=b:g(b:c)
g[a,0]=[a,a]
g(a:b)=a:g b
q l=sum.map(sum.zipWith(!)l).take 3
c!n|s c||c=='.'=0|1>0=n
h i=show$sum$zipWith q i$tails$[0,0..]:map(g.f)i
main=interact$h.lines