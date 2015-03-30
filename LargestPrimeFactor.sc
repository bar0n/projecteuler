import scala.collection.immutable.Stream
def fromLong(start: Long, step: Long): Stream[Long] =
  start #:: fromLong(start + step, step)
def largestPrimeFactor(num: Long): Long = {

  def next(st: Long, curNum: Long, res: Vector[Long]): Vector[Long] = {
    val filter: Stream[Long] = fromLong(st, 2).filter(x => res.exists(y => x % y == 0))
    println(filter.take(10).toList)
    val take: Long = filter.take(1)(1)
    val newCur: Long = curNum / take
    if (newCur > 1) next(take, newCur, take +: res)
    else res
  }
  next(3, num, Vector(2)).head
}

largestPrimeFactor(13195)