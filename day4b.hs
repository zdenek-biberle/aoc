import Data.List
q(m:n)l|p<-words l,(a,b)<-splitAt(length$p\\nub p)n=(map(+m)a<>b,m)
main=interact$show.sum.snd.mapAccumL q[1,1..].lines