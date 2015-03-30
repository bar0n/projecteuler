/**
 * Created by dbaron on 20.01.15.
 */
object Pr009_SpecialPythagoreanTriplet {
  def main(arg: Array[String]): Unit = {
    val d = for {
      d1 <- 1 to 1000
      d2 <- 1 to 1000
      d3 <- 1 to 1000
      if d1 + d2 + d3 == 1000 && d1 * d1 + d2 * d2 == d3 * d3
    } yield (d1*d2*d3)
    println(d)
  }
}
