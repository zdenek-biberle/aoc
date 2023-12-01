import Text.ParserCombinators.ReadP
r=reverse
a!x=head.filter(<':').fst.head.readP_to_S((many$choice(zipWith((.pure).(>>).string.a)["one","two","three","four","five","six","seven","eight","nine"]['1'..'9'])<++get)<*eof)$x
main=interact$show.sum.map(\x->read[id!x,r!r x]).lines