import scala.annotation.tailrec
import scala.collection.immutable.Nil
def rest_(x: Int, y: Int): Boolean = y % x == 0
def rest = (x: Int) => rest_(x, _: Int)
def isPrime(n: Int, fs: List[(Int => Boolean)]) = {
  val res = fs.forall(x => !x(n))
  res
}
def isPrime(n: Int, fs: List[(Int => Boolean)], primes: List[Int]) = {
  val count: Int = primes.count(x => x < n)
  val res = fs.takeRight(count).forall(x => !x(n))
  res
}
def intToList(int: Int): List[Int] = int.toString.map(x => Integer.parseInt(x.toString)).toList
def listToInt(list: List[Int]): Int = {
  def req(l: List[Int], sum: Int, osn: Int): Int = l match {
    case Nil => sum
    case _ => req(l.init, sum + osn * l.last, osn * 10)
  }
  req(list, 0, 1)
}
def circularPerm(list: List[Int]): List[List[Int]] = {
  val seq = 1 until list.size
  seq.map(x => {
    val sp = list.splitAt(x)
    sp._2 ++ sp._1
  }).toList
}

def isPermutationsPrime(int: Int, fs: List[(Int => Boolean)], primes: List[Int]): Boolean = {
  circularPerm(intToList(int)).map(listToInt).forall(x => isPrime(x, fs, primes))
}
@tailrec
def getNextPrime(fs: List[(Int => Boolean)], N: Int): Int = {
  /*def isPrime(n: Int) = {
    val res = fs.forall(x => !x(n))
    res
  }*/
  if (isPrime(N, fs)) N
  else getNextPrime(fs, N + 2)
}
@tailrec
def getListOfPrimes(fs: List[(Int => Boolean)], primes: List[Int], limit: Int): (List[Int], List[(Int => Boolean)]) = {
  if (primes.head < limit) {
    val next = getNextPrime(fs, primes.head + 2)
    getListOfPrimes(rest(next) :: fs, next :: primes, limit)
  }
  else (primes, fs)
}
val functions: List[(Int) => Boolean] = List(rest(7), rest(5), rest(3), rest(2))
val ints: List[Int] = List(7, 5, 3, 2)
val (listOfPrimes, fs) = getListOfPrimes(functions, ints, 1000000)
val f = listOfPrimes.filter(x => isPermutationsPrime(x, fs, listOfPrimes))
f.size

