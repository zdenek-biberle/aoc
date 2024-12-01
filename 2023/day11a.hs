import Data.List
i!l=i+head([1|'#'<-l]<>[2])
t=scanl(!)0
f w=sum[abs(a-c)+abs(b-d)|((a,b):p)<-tails[(x,y)|(y,l)<-zip(t w)w,(x,'#')<-zip(t$transpose w)l],(c,d)<-p]
main=interact$show.f.lines