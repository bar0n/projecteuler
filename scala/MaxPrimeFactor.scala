import scala.collection.immutable.Stream

/**
 * Created by dbaron on 19.01.15.
 */
object MaxPrimeFactor {

  def fromLong(start: Long, step: Long): Stream[Long] =
    start #:: fromLong(start + step, step)

  def largestPrimeFactor(num: Long): Long = {



    def next(st: Long, curNum: Long, res: Vector[Long]): Vector[Long] = {
      def exist(n: Long): Boolean = !res.exists(y => n % y == 0)
      val take: Long = fromLong(st, 2).filter(exist).take(1).head
      val newCur: Long = if (curNum % take == 0) curNum / take else curNum
      if (newCur > 1) next(take, newCur, take +: res)
      else take +: res
    }
    val vect = next(3, num, Vector(2))
    print(vect)
    vect.head
  }

  def main(args: Array[String]) {
    println(largestPrimeFactor(2520l))
    // println(largestPrimeFactor(600851475143l))
    //println(sieveOfEratosthenes(600851475143l))
    //  println(getRange(2l, 600851475143l).size)
    //println((2l to 600851475143l).size)

  }
}
