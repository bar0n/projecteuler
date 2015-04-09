val seq = (1 until 10).map(1 to _).flatMap(_.permutations.toList.map(x => Integer.parseInt(x.mkString("")))).filter(_ % 2 == 1)

val N = math.sqrt(seq.max).toInt

def prime(stream: Stream[Int]): Stream[Int] = {
  stream.head #:: prime(stream.tail.filter(x => x % stream.head != 0))
}

val primes = prime(Stream.from(2)).takeWhile(x => x < N).toList

seq.filter(x => !primes.exists(y => y < x && x % y == 0)).max

