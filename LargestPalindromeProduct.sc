import scala.collection.immutable.IndexedSeq

def isPalindrom(num: Int): Boolean = {
  val string: String = num.toString
  val half = string.length / 2
  string.take(half) == string.takeRight(half).reverse
}

/*(for {
  x <- 10 to 99
  y <- 10 to 99
  if isPalindrom(x * y)
} yield x * y).max*/

def stream(num: Int, st: Stream[Int]): Stream[Int] = {
  num #:: stream(num - 1, num #:: st)
}
/*def canDivide(num: Int): Boolean = {
  val range = 100 to 999 zip List(899,num)

}*/
/*
def isProduct(int: Int): Boolean = {
  def rest(num: Int): IndexedSeq[Int] = {
    val range = 100 to 999 zip Seq.fill(900)(num)
    range.filter({ case (x, y) => y % x == 0}).map({ case (x, y) => y / x}).filter(x => x >= 100 && x <= 999)
  }
  val list = rest(int)
  list.map(rest).map(x => x.map(rest).filter(x => x.nonEmpty)).flatten.nonEmpty
}
val d = stream(997002999, Stream.Empty)
val palindrome = d.filter(isPalindrom)
palindrome.filter(isProduct).take(1).toList
*/


def rest(num: Int): IndexedSeq[Int] = {
  val range = 100 to 999 zip Seq.fill(900)(num)
  range.filter({ case (x, y) => y % x == 0}).map({ case (x, y) => y / x}).filter(x => x >= 100)
}
val list = rest(996969699)
list zip list.map(x => 996969699 / x)
val prod = for {
  d <- rest(996969699)
  f <- rest(d)
//if f <= 999
} yield (f, d / f, 996969699 / d)



