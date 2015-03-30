import java.util.Date

import scala.collection.immutable.Nil

/**
 * Created by dbaron on 20.01.15.
 */
object pr_012_TriangularNumber_2 {

  var primes: Vector[Long] = Vector(3, 2)
  var cout = 1

  def main(args: Array[String]) {

    def isPrime(int: Long): Boolean = {

      val filter: Vector[Long] = primes.filter(_ <= int)
      if (filter.forall(int % _ != 0) && filter.nonEmpty) {
        primes = int +: primes
        true
      } else false
    }

    def combination(list: List[List[(Long, Int)]]): List[List[(Long, Int)]] = {
      list match {
        case Nil => List(Nil)
        case head :: tl =>
          for {
            i <- head
            tail <- combination(tl)
          } yield i :: tail
      }
    }

    def pows(n: Long, pow: Int): Long =
      (for (l <- 1 to pow) yield n).foldLeft(1l)((x, y) => x * y)

    def getDivisors(n: Long, map: Map[Long, Int]): Map[Long, Int] = {
      if (n == 1) map
      else primes.find(x => n % x == 0) match {
        case Some(x) => getDivisors(n / x, map.updated(x, map.getOrElse(x, 0) + 1))
        case None => map
      }
    }

    def fillPrimes(n: Long): Int = {
      if (isPrime(n)) {
        1
      }
      else
      if (n > 1) fillPrimes(primes.filter(n % _ == 0).foldLeft(n)((x, y) => n / y))
      else 1
    }

    def countDivider(N: Long): (Int, Long) = {

      fillPrimes(N)

      val m = getDivisors(N, Map()).toList

      val nums = m.foldLeft(1)((x, y) => x * (y._2 + 1))

      /*val pow = (for {
        (prime, power) <- m
        k <- 0 to power
      } yield (prime, k)).groupBy({ case (pr, k) => pr}).map({ case (i, j) => j}).toList

      val comb: List[List[(Long, Int)]] = combination(pow)
      (comb.size, N)*/
      (nums, N)

    }

    var map = scala.collection.mutable.Map(1l -> 1)

    def sumN(n: Int): Long = n * (n + 1) / 2

    val triangle = Stream.from(2).map(sumN)

    println(new Date)
    val res = triangle.map(countDivider).takeWhile(y => y._1 <= 601).toVector
    println(res.filter(y => y._1 >= 500))
    println(res.last)
    //println(sumN(res.size))
    println(new Date)

  }
}
