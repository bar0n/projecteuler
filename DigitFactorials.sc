val map = Map(9->1*2*3*4*5*6*7*8*9,
8->1*2*3*4*5*6*7*8,
7->1*2*3*4*5*6*7,
6->1*2*3*4*5*6,
5->1*2*3*4*5,
4->1*2*3*4,
3->1*2*3,
2->1*2,
1->1,
0->1)

val n = 5
map(9)*n>9*math.pow(10,n)

val seq = 3 to 999999

def filter(int:Int):Boolean = {
  int.toString.map(x=>map(Integer.parseInt(x.toString))).sum == int
}
seq.filter(filter)
seq.filter(filter).sum
filter(40585)
40585.toString.map(x=>map(Integer.parseInt(x.toString))).sum
40585.toString.map(x=>map(Integer.parseInt(x.toString)))