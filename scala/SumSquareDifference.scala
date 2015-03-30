/**
 * Created by dbaron on 20.01.15.
 */
object SumSquareDifference {
  def sumDif(n: Int): Long = {
    val n2 = n * n
    val n3 = n2 * n
    val n4 = n2 * n2
    n4 / 4 + n3 / 6 - n2 / 4 - n / 6
  }

  def main(arg: Array[String]): Unit = {
    println(sumDif(100))
  }
}
