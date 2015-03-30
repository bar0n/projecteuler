val N = 192
val s = 2 to 9
val seq = s.map(x => 1 to x toVector)
val source = 1 to 999
val vect = seq(0)
val n: Int = 9 / vect.size
n * vect.size
val v = Vector.fill(vect.size)(n)
val fil = v.updated(vect.size - 1, v.last + 9 - n * vect.size)
fil.map(x => math.pow(10, x - 1).toInt)
def fl(int: Int): Boolean = {

}
source.filter(fl)
/*
def findNs(vect:Vector[Int]):(Int,Int) ={
  val n:Int =  9/vect.size;

  vect.map(x=>)
}*/
