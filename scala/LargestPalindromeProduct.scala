import scala.annotation.tailrec
import scala.collection.immutable.IndexedSeq

/**
 * Created by dbaron on 19.01.15.
 */
object LargestPalindromeProduct {

  def isPalindromeRec(s: String) = {
    @tailrec
    def inner(s: String): Boolean =
      (s.length <= 1) || (s.head == s.last) && inner(s.tail.init)

    (s.length >= 2) && inner(s)
  }

  def isPalindrom(num: Int): Boolean =  isPalindromeRec(num.toString)

  def stream(num: Int, st: Stream[Int]): Stream[Int] = {
    num #:: stream(num - 1, num #:: st)
  }

  def isProduct(int: Int): Boolean = {
    println("test " + int)
    def rest(num: Int): IndexedSeq[Int] = {
      val range = 100 to 999 zip Seq.fill(900)(num)
      range.filter({ case (x, y) => y % x == 0}).map({ case (x, y) => y / x}).filter(x => x >= 100)
    }
    val prod = for {
      d <- rest(int)
      //f <- rest(d)
      if d <= 999
    } yield (d, int / d)
    if (prod.nonEmpty) println(prod)
    prod.nonEmpty
  }

  def getNextPalindrom(num: Int): Int = {
    val palindrome = stream(num - 1, Stream.Empty).filter(isPalindrom)
    palindrome.take(1).toList(0)
  }


  def palindrom(num: Int): Long = {
    var i = num
    while (!isProduct(i)) {
      i = getNextPalindrom(i)
    }
    i
  }

  def main(arg: Array[String]): Unit = {
    val i = 999 * 999
    println(palindrom(getNextPalindrom(i)))
   // println("isPalindrom " + isPalindrom(997002999)) //967262769
    /*
        val palindrome = streamPalindrom(i, Stream.Empty)
        println(palindrome.filter(isProduct).take(1).toList)
    */
  }
}
