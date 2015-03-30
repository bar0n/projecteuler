import scala.collection.immutable.Nil
def prime(stream: Stream[Int]): Stream[Int] = stream.head #:: prime(stream.tail.filter(x => x % stream.head != 0))
val primes1 = prime(Stream.from(2))
val primes = primes1.take(100).toList
val m = getDivisors(600, Map()).toList
def getDivisors(n: Int, map: Map[Int, Int]): Map[Int, Int] = {
  if (n == 1) map
  else primes.find(x => n % x == 0) match {
    case Some(x) => getDivisors(n / x, map.updated(x, map.getOrElse(x, 0) + 1))
    case None => map
  }
}

val pow = (for {
  (prime, power) <- m
  k <- 0 to power
} yield (prime, k)).groupBy({ case (pr, k) => pr}).map({ case (i, j) => j}).toList
def combination(list: List[List[(Int, Int)]]): List[List[(Int, Int)]] = {
  list match {
    case Nil => List(Nil)
    case head :: tl =>
      for {
        i <- head
        tail <- combination(tl)
      } yield i :: tail
  }
}
def pows(n: Int, pow: Int): Int =
  (for (l <- 1 to pow) yield n).foldLeft(1)((x, y) => x * y)
pows(2, 4)
val comb = combination(pow)

def prod(prod: List[(Int, Int)]): Long = {
  prod.foldLeft(1l)({ case (x, (mant, pw)) => x * pows(mant, pw)})
}

val c1=comb.map(prod).sorted
val c2=List(1, 2, 3, 4, 5, 6, 8, 10, 12, 15, 20, 24, 25, 30, 40, 50, 60, 75, 100, 120, 150, 200, 300, 600)
c1==c2
//combination(pow).map(x => x.foldLeft(1)((x,y)=>pows(x,y)))
