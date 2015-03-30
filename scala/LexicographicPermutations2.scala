/**
 * Created by dbaron on 30.01.15.
 */
object LexicographicPermutations2 {
  val perm = Vector(
    "0123",
    "0132",
    "0213",
    "0231",
    "0312",
    "0321",
    "1023",
    "1032",
    "1203",
    "1230",
    "1302",
    "1320",
    "2013",
    "2031",
    "2103",
    "2130",
    "2301",
    "2310",
    "3012",
    "3021",
    "3102",
    "3120",
    "3201",
    "3210")
  def getNextPermutation(list: Vector[Int]): Vector[Int] = {
    val zip: Vector[(Int, Int)] = list zip (list.tail :+ -1)
    val (l, r) = lastSpan(zip, (x: (Int, Int)) => x._1 > x._2)
    val left = (l unzip)._1
    val right = (r unzip)._1
    if (left.isEmpty) right
      else {
      val rev = right.reverse
      val ind = rev.indexWhere(x => x >= left.last + 1)
      (left.init :+ rev(ind)) ++ rev.updated(ind, left.last)
    }
  }

  def lastSpan(list: Vector[(Int, Int)], p: ((Int, Int)) => Boolean): (Vector[(Int, Int)], Vector[(Int, Int)]) = {
    def req(xs: Vector[(Int, Int)], int: Int): Int = {
      if (xs.isEmpty) int
      else  if (p(xs.last)) req(xs.init, int - 1)
      else int
    }
    val req1: Int = req(list, list.size)
    list.splitAt(req1)
  }
  val list = Vector(1, 0, 4, 3, 2)
  getNextPermutation(Vector(0, 4, 3, 2, 1))
  getNextPermutation(list)

  def permutationStream(init: Vector[Int], stream: Stream[Vector[Int]]): Stream[Vector[Int]] = {
    val permutation: Vector[Int] = getNextPermutation(init)
    init #:: permutationStream(permutation, permutation #:: stream)
  }



  def main(args: Array[String]) {
    val streamPerm = permutationStream(Vector(0, 1, 2, 3, 4, 5, 6, 7, 8 , 9),Stream()) //, 3, 4, 5, 6, 7, 8 , 9
    val toVector: Vector[Vector[Int]] = streamPerm.take(1000000).toVector
    println(toVector.map(x=>x.mkString).mkString("\n"))
   // println(perm.distinct.sorted.mkString(","))
    println()
    println(toVector.last)

  }
}
