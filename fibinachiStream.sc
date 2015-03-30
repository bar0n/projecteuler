import scala.collection.immutable.Stream.Empty

def next(fb: Stream[BigInt]): Stream[BigInt] = {
  val sum = fb.take(2).sum
  sum #:: next(sum #:: fb)
}
def fib(): Stream[BigInt] = BigInt(1) #:: BigInt(2) #:: next(BigInt(2) #:: BigInt(1) #:: Empty)
fib().filter(_ % 2 == 0).takeWhile(_ < BigInt(4000000)).sum

