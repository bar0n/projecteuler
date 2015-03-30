import scala.collection.immutable.IndexedSeq

/**
 * Created by dbaron on 03.02.15.
 */
object QuadraticPrimes {
  def main(args: Array[String]) {
    val seq = for {
      a <- -1000 to 1000
      b <- -1000 to 1000
    } yield (a, b)

    def isPrime(int: Long): Boolean = {

      if (int < 0) false
      else {
        val abs = math.abs(int)
        if (abs % 2 == 0) false
        else
          (3 to math.sqrt(int).toInt by 2).forall(abs % _ != 0)
      }
    }

    def quadratic(a: Int, b: Int): Int = {

      def f(int: Int) = int * int + a * int + b

      def req(int: Int, sum: Int): Int = {
       // print(f(int) + ", ")
        if (isPrime(f(int))) req(int + 1, sum + 1)
        else sum
      }
      val req1: Int = req(1, 0)
      if (a==1 &&b==41) println("req1="+req1)
      req1
    }
  /*println(quadratic(1, 41))
    println(quadratic(-79, 1601))
    println(quadratic(-97, -23))*/
    val map: IndexedSeq[Int] = seq.map({ case (a, b) => quadratic(a, b)})
    val max: Int = map.max
    val of: Int = map.indexOf(max)

    val seq1: (Int, Int) = seq(of)
    println(""+seq1+" "+seq1._1*seq1._2)
    println(quadratic(seq1._1,seq1._2))
    println("done")
  }
}
