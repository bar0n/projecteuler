Stream.from(1)
Stream.from(1).map(x=>(x,x.toString.length))
Stream.from(1).map(x=>(x,x.toString.length)).take(100).toList

val d= Stream.from(1).flatMap(x=>x.toString.map(y=>Integer.parseInt(y.toString))).take(1000000).toList

//d1 × d10 × d100 × d1000 × d10000 × d100000 × d1000000
d(1-1)
d(10-1)
d(100-1)
d(1000-1)
d(10000-1)
d(100000-1)
d(1000000-1)


d(1-1)*
d(10-1)*
d(100-1)*
d(1000-1)*
d(10000-1)*
d(100000-1)*
d(1000000-1)