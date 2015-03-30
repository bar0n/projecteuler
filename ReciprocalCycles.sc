import scala.collection.immutable.IndexedSeq

/*A unit fraction contains 1 in the numerator. The decimal representation of the unit fractions with denominators 2 to 10 are given:

1/2	= 	0.5
1/3	= 	0.(3)
1/4	= 	0.25
1/5	= 	0.2
1/6	= 	0.1(6)
1/7	= 	0.(142857)
1/8	= 	0.125
1/9	= 	0.(1)
1/10	= 	0.1

Where 0.1(6) means 0.166666..., and has a 1-digit recurring cycle. It can be seen that 1/7 has a 6-digit recurring cycle.

  Find the value of d < 1000 for which 1/d contains the longest recurring cycle in its decimal fraction part.
*/
val seq = 1 to 1000
def divide(int: Int, div: Int): Vector[Int] = Vector()

def pow(int: Int, n: Int, prod: Int): Int = {
  if (n == 0) prod
  else
  if (n % 2 == 0) pow(int * int, n / 2, prod)
  else pow(int, n - 1, prod * int)
}
def power(i: Int, N: Int) = pow(i, N, 1)

def rest(div: Int): List[Int] = {
  val log = (math.log10(div) + 0.5).round.toInt
  val N = power(10, log)

  def requr(r: Int, rests: List[Int], divider: List[String]): List[String] = {
    val i: Int = r % div
    if (rests.contains(i)) divider
    else requr(N * i, rests :+ i, divider :+ ("%0" + log + "d").format(N * i / div))
  }
  val srt = requr(N % div, Nil, List(("%0" + log + "d").format(N / div)))
  srt.init.flatMap(x => x.map(y => Integer.parseInt(y.toString)))
}
def restCount(div: Int): Int = rest(div).size
val div = 13
rest(div)
val d: BigDecimal = BigDecimal(1) / div
seq.map(rest)
val map: IndexedSeq[Int] = seq.map(restCount)
map.max
val ind = map.indexOf(map.max)
seq(ind)