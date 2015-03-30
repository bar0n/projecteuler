val seq = for {
  a <- -100 to 100
  b <- -100 to 100
} yield (a, b)

def primesStream(stream: Stream[Int]): Stream[Int] = {
  stream.head #:: primesStream(stream.tail.filter(x => x % stream.head != 0))
}
val primes = primesStream(Stream.from(2)).take(10000).toVector

def quadratic(a: Int, b: Int): Int = {

  def f(int: Int) = int * int + a * int + b

  val firstPrimeInd: Int = primes.indexOf(f(1))
  def req(int: Int, sum: Int): Int = {
    //println("int=" + int + " f(int)=" + f(int))
    print(f(int)+", ")
    if (primes.contains(f(int))) req(int + 1, sum + 1)
    else sum
  }
  req(1, 0)
}
quadratic(1, 41)

