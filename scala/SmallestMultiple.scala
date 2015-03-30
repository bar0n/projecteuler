/**
 * Created by dbaron on 20.01.15.
 */
object SmallestMultiple {

  def prime(stream: Stream[Int]): Stream[Int] = {
    val head: Int = stream.head
    head #:: prime(stream.tail.filter(x => x % head != 0))
  }

  def gcd(a: Int, b: Int): Int = {
    if (b == 0) a else gcd(b, a % b)
  }

  def div(num: Int, prim: List[Int], res: List[Int]): List[Int] = {
    if (num <= 1) res
    else if (num % prim.head == 0) div(num / prim.head, prim, res :+ prim.head)
    else div(num, prim.tail, res)
  }

  def smallestMultiple(n: Int): Int = {
    val d = 2 to n
    def cond(int: Int): Boolean = {
      print(int)
      d.forall(y => int % y == 0)
    }
    Stream.from(n).filter(x => d.forall(y => x % y == 0)).take(1).toList(0)
  }

  def printSmal(n: Int): Unit = {
    val divider = 2 to n

    val prim: List[Int] = prime(Stream.from(2)).take(100).toList
    val small = smallestMultiple(n)
    println(n)
    println(divider)
    println(small)
    println(div(small, prim, Nil))
    println(divider.map(x => div(x, prim, Nil)))
  }

  def work(int: Int): List[Int] = {
    val prim: List[Int] = prime(Stream.from(2)).take(int).toList
    def as(n: Int, list: List[Int]): List[Int] = {
      if (n > int) list
      else {
        val flatten = div(n + 1, prim, Nil)
        val res = list ++ flatten.diff(list)
        println(n)
        println(2 to n)
        println(flatten)
        println(res)
        println(res.foldLeft(1)((x, y) => x * y))
        println()
        as(n + 1, res)
      }
    }
    as(2, List(2))
  }


  def main(args: Array[String]): Unit = {

    work(20)



    /*   val stream = Stream.from(10)

       def exist(int: Int): Boolean = {

         val bol = divider.forall(y => int % y == 0)

         //   println("eximine = " + int + "  bol=" + bol)
         //if (int > 3000) System.exit(0)
         bol
       }*/
    //val fil = stream.filter(exist)
    // val res = fil.take(1).toList
    // println(divider.foldLeft(1L)((x, y) => x * y))
    // println(gcd(3628800, 2520))
  }
}
