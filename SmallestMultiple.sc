val divider = 1 to 10

val stream = Stream.from(10)
def exist(int: Int): Boolean = !divider.exists(y => int % y == 0)
val fil = stream.filter(exist)
//fil.take(1).toList

def prime(stream: Stream[Int]): Stream[Int] = {
  val head: Int = stream.head
  head #:: stream.tail.filter(x => x % head == 0)
}

prime(Stream.from(2)).take(20).toList



