import scala.annotation.tailrec
import scala.collection.immutable.Nil

def rest_(x: Int, y: Int): Boolean = y % x == 0
def rest = (x: Int) => rest_(x, _: Int)
def isPrime(n: Int, fs: List[(Int => Boolean)]) = {
  val res = fs.forall(x => !x(n))
  res
}
def isPrime(n: Int, fs: List[(Int => Boolean)], primes: List[Int]) = {
  if (n==1) false
  else {
    val count: Int = primes.count(x => x < n)
    val res = fs.takeRight(count).forall(x => !x(n))
    res
  }
}

def intToList(int: Int): List[Int] = int.toString.map(x => Integer.parseInt(x.toString)).toList
def listToInt(list: List[Int]): Int = {
  def req(l: List[Int], sum: Int, osn: Int): Int = l match {
    case Nil => sum
    case _ => req(l.init, sum + osn * l.last, osn * 10)
  }
  req(list, 0, 1)
}

def candidats(list: List[Int]) = (1 until list.size).flatMap(x => {
  val (left, right) = list.splitAt(x)
  List(left, right)
})

def isPrimeTruncatable(pr: Int, fs: List[(Int => Boolean)], primes: List[Int]): Boolean = {
  candidats(intToList(pr)).map(listToInt).forall(x => isPrime(x, fs, primes))
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
def getListOfPrimes(fs: List[(Int => Boolean)], primes: List[Int], limit: Int, trPrimes: List[Int]): (List[Int], List[Int]) = {
  if (trPrimes.size < limit) {
    val next = getNextPrime(fs, primes.head + 2)
    if (isPrimeTruncatable(next, fs, primes))
      getListOfPrimes(rest(next) :: fs, next :: primes, limit, next :: trPrimes)
    else
      getListOfPrimes(rest(next) :: fs, next :: primes, limit, trPrimes)
  }
  else (primes, trPrimes)
}
val functions: List[(Int) => Boolean] = List(rest(7), rest(5), rest(3), rest(2))
val ints: List[Int] = List(7, 5, 3, 2)
val (listOfPrimes, fs) = getListOfPrimes(functions, ints, 11, List())
fs.sum




