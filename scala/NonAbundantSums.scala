import scala.collection.immutable.IndexedSeq

/**
 * Created by dbaron on 29.01.15.
 */
object NonAbundantSums {
  def main(args: Array[String]) {
    val seq = 1 to 28123
    def divisors(int: Int): List[Int] = {
      (1 to int / 2).filter(x => int % x == 0).toList
    }

    def sumDivisors(int: Int): Int = divisors(int).sum

    def isAbundant(int: Int): Boolean = sumDivisors(int) > int

    val seqAbudant = seq.filter(isAbundant)

    val numSumAbudant = for {
      abud1 <- seqAbudant
      abud2 <- seqAbudant
      if abud1 <= abud2 && (abud1 + abud2) <= 28123
    } yield abud1 + abud2

    val diff: IndexedSeq[Int] = seq.diff(numSumAbudant)
    println("diff = "+diff)
    println("sum=" + diff.sum)

  }
}
