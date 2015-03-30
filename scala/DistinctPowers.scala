/**
 * Created by dbaron on 05.02.15.
 */
object DistinctPowers {
  def main(args: Array[String]) {
    def power(osn: BigInt, step: BigInt, pow: BigInt): BigInt =
      if (step == 0) pow
      else if (step % 2 == 0) power(osn * osn, step / 2, pow)
      else power(osn, step - 1, pow * osn)
    def f(a: Int, b: Int, c: Int, d: Int): Boolean =
      power(a, b, BigInt(1)) == power(c, d, BigInt(1))
    val n = 100
    val seq = 2 to n

    val d = (for {
      a <- seq
      b <- seq
      c <- seq
      d <- seq
      if a < c && d < b && f(a, b, c, d)
    } yield (a,b)).distinct
    //(power(a, b , BigInt(1)),power(c,d,BigInt(1)))
    /*(n - 1) * (n - 1)
    d.size
    (n - 1) * (n - 1) - d.size*/
    println((n - 1) * (n - 1) - d.size)
  }
}
