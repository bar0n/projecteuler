import scala.collection.immutable.{Nil, IndexedSeq}

def power(osn: Long, step: Long, pow: Long): Long =
  if (osn == 0 || osn == 1) osn
  else
  if (step == 0) pow
  else if (step % 2 == 0) power(osn * osn, step / 2, pow)
  else power(osn, step - 1, pow * osn)
def pow(l: Long, int: Int) = power(l, int, 1)
val N = 5
val pow1: Long = pow(10, N)
val seq = 1l until pow1

def isSum(int: Long): Boolean = {
  val map: List[Int] = int.toString.map(x => Integer.parseInt(x.toString)).toList
  def reqSum(m: List[Int], sum: Long): Boolean = {
   // println("sum="+sum+" m="+m)
    m match {

      case Nil =>
        //println("sum="+sum)
       // println("sum == int="+(sum == int))
        sum == int
      case ::(head, tl) => if (sum <= int) reqSum(tl, sum + pow(head, N)) else false
    }
  }
  reqSum(map,0)
}
Vector(4151, 54748, 92727, 93084, 194979).sum
//4150, 4151, 54748, 92727, 93084, 194979.
pow(4,5)+pow(1,5)+pow(5,5)+pow(0,5)
isSum(4150)
val filter: IndexedSeq[Long] = seq.filter(isSum)
filter.sum-1
Vector( 4150, 4151, 54748, 92727, 93084, 194979).sum
//seq.filter(isSum).sum-1