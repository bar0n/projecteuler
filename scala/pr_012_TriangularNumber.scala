import java.util.Date

/**
 * Created by dbaron on 20.01.15.
 */
object pr_012_TriangularNumber {

  var primes: Vector[Long] = Vector(3, 2)
  var cout = 1

  def main(args: Array[String]) {

    def countDivider(N: Long): Int = {
      cout = cout + 1
      if (cout % 200 == 0) {
        println(N)
        println(primes.take(10))
      }
      def isPrime(int: Long): Boolean = {
        val d = math.sqrt(int)
        if (primes.filter(_ < d).forall(int % _ != 0)) {
          primes = int +: primes
          true
        } else false
      }

      val filter: Vector[Long] = primes.filter(N % _ == 0)
      val rest = filter.foldLeft(N)((x, y) => N / y)
      if (rest > 1) {
        val newPrimes = for {
          pr <- primes.head + 2 to rest by 2
          if isPrime(pr)
        } yield pr
        newPrimes.size + filter.size
      } else filter.size
    }
    var map = scala.collection.mutable.Map(1l -> 1)

    def countDivider2(N: Long): Int = {

      val (n1, n2) = if (N % 2 == 0) (N / 2, N + 1) else (N, (N + 1) / 2)

      val first = map.getOrElse(n1, {
        val newDiv = countDivider(n1)
        map = map + (n1 -> newDiv)
        newDiv
      })
      val second = map.getOrElse(n2, {
        val newDiv = countDivider(n2)
        map = map + (n2 -> newDiv)
        newDiv
      })

      first + second
    }


    def sumN(n: Int): Long = n * (n + 1) / 2

    val triangle = Stream.from(1).map(sumN)

    println(triangle.take(7).toList)

    // Stream.from(2).map(x => x.asInstanceOf[Long]).map(countDivider2).takeWhile(y => y < 501).toVector

    //  val count = triangle.map(countDivider).takeWhile(y => y < 501).toVector
    println(new Date) //Wed Jan 21 10:39:53 EET 2015
    //    println(primes.take(10))
    //   println(count.last)

    //println(2 to 10 zip (2 to 10 map sumN) zip (2 to 10).map(x => x.asInstanceOf[Long]).map(countDivider2))
    println(countDivider(4))

    println(new Date)

  }
}

object test {
  def main(args: Array[String]) {
    def prime(stream: Stream[Int]): Stream[Int] = stream.head #:: prime(stream.tail.filter(x => x % stream.head != 0))
    val primes1 = prime(Stream.from(2))
    val primes = primes1.take(100).toList
    def getDivisors(n: Int, map: Map[Int, Int], set: List[Int]): List[Int] = {
      if (n == 1) set
      else primes.find(x => n % x == 0) match {
        case Some(x) => {
          getDivisors(n / x, map.updated(x, map.getOrElse(x, 0) + 1), n :: set)
        }
        case None => set
      }
    }
    val m = getDivisors(48, Map(), List()).toList
    println(m)
  }
}
