import scala.collection.immutable.IndexedSeq

def primesStream(stream: Stream[Int]): Stream[Int] =
  stream.head #:: primesStream(stream.filter(x => x % stream.head != 0))

val primes = primesStream(Stream.from(2)).take(1000).toList

def numR(prime: Int, N: Int): Int = {
  def count(m: Int, sum: Int): Int =
    if (m % prime == 0) count(m / prime, sum + 1)
    else sum
  count(N, 0)
}

val list = 1 to 10000

def divisors(N: Int): List[Int] =
    1 :: list.tail.takeWhile(x => x <= N / 2).toList.filter(x => N % x == 0)

val pairs = list zip list.map(divisors).map(x=>x.sum)

for{
  (k, v) <- pairs
  (k1, v2) <- pairs
  if k != k1
  if k == v2 && k1 ==v
} yield k


Vector(220, 284, 1184, 1210, 2620, 2924, 5020, 5564, 6232, 6368).sum

