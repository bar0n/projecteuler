import scala.annotation.tailrec

/**
 * Created by dbaron on 20.01.15.
 */
object pr_010 {

  type fil = (Long) => Boolean

  def multiple_of = (base: Long, target: Long) => target % base == 0

  def filter = (x: Long) => multiple_of(x, _: Long)

  //

  def relatively_prime(fs: List[(Long) => Boolean], target: Long): Boolean = {
    for (f <- fs) {
      if (f(target)) {
        return false
      }
    }
    true
  }

  @tailrec
  def next_prime(fs: List[(Long) => Boolean], x: Long): Long = {

    if (relatively_prime(fs, x)) x
    else next_prime(fs, x + 2)
  }

  @tailrec
  def primes(fs: List[(Long) => Boolean], ps: List[Long], limit: Long): List[Long] = {
    val np = next_prime(fs, ps.head + 2)

    if (np > limit) ps
    else primes(fs :+ filter(np), np :: ps, limit)
  }

  def main(args: Array[String]) {
    val f = primes(List(filter(2), filter(3)), List(3, 2), 2000000)
    println(f)
    println(f.sum) //1179908154
    /*   val fs = List(filter(2), filter(3))
       for (f <- fs) {
         println("Eight is a multiple of this filter: " + f(8))
       }
       println("4 is prime? " + relatively_prime(fs, 4))
       println("5 is prime? " + relatively_prime(fs, 5))

       val list = List[Long](2, 3, 4, 5, 6, 7, 8, 9, 10, 11)
       println(list.map(x => relatively_prime(fs, x)))

       println(next_prime(fs, 4))
       println(next_prime(fs, 8))
   */
    /*def prime(stream: Stream[Long]): Stream[Long] = stream.head #:: prime(stream.tail.filter(x => x % stream.head != 0))
    println(new Date)
    println(prime(Stream.from(2)).takeWhile(x => x < 2000000).sum)*/
  }
}
