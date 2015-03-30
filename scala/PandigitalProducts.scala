/**
 * Created by dbaron on 09.02.15.
 */

object PandigitalProducts {

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
      else if (p(xs.last)) req(xs.init, int - 1)
      else int
    }
    val req1: Int = req(list, list.size)
    list.splitAt(req1)
  }


  def main(args: Array[String]) {
    var vect = Vector(1, 2, 3, 4, 5, 6, 7, 8, 9)
    val reverse: Vector[Int] = vect.reverse
    var set: Set[(Int, Int)] = Set()


    def numb(v: Vector[Int]): Int = {
      def num(vect: Vector[Int], s: Int, prod: Int): Int = if (vect.isEmpty) prod else num(vect.init, s * 10, prod + vect.last * s)
      num(v, 1, 0)
    }

    def prod(i: Int, j: Int, vec: Vector[Int]): (Int, Int, Int) = {
      val (ix, ixz) = vec.splitAt(i)
      val (iy, iz) = ixz.splitAt(j - i)
      (numb(ix), numb(iy), numb(iz))
    }


    def insertWhenNeed(a: Int, b: Int, c: Int): Boolean = {
      (a * b == c)
      // println(""+a+"*"+b+"="+c)
      /* if (!set.contains(c)) {
         if (a * b == c) {
           //set = set + (c)
           true
         } else false
       } else false*/
    }
    def checkPermutation(vector: Vector[Int]): Int = {
      val d = for {
        i <- 1 to vector.length - 2
        j <- i + 1 to vector.length - 1
        (a, b, c) = prod(i, j, vector)
        if insertWhenNeed(a, b, c)
      } yield (a, b, c)
      if (d.nonEmpty) println(d)
      d.map({ case (_, _, c) => c}).sum
    }

    var sum: Long = checkPermutation(vect)
    //checkPermutation(Vector(1,2,3,4,5,6,7,8,9))
    do {
      vect = getNextPermutation(vect)
      sum = sum + checkPermutation(vect)
      // println(vect)
    } while (reverse != vect)
    println(set)
    println(sum)

    Vector((12, 483, 5796))
    Vector((138, 42, 5796))
    Vector((157, 28, 4396))
    Vector((159, 48, 7632))
    Vector((1738, 4, 6952))
    Vector((18, 297, 5346))
    Vector((186, 39, 7254))
    Vector((1963, 4, 7852))
    Vector((198, 27, 5346))


    //  Vector((27,198,5346))
    //  Vector((28,157,4396))
    // Vector((297,18,5346))
    //   Vector((39,186,7254))
    // Vector((4,1738,6952))
    // Vector((4,1963,7852))
    //  Vector((42,138,5796))
    //  Vector((48,159,7632))
    // Vector((483,12,5796))
    println(Vector(5796, 5796, 4396, 7632, 6952, 5346, 7254, 7852, 5346).sum)
    println(Vector(5796, 4396, 7632, 6952, 5346, 7254, 7852).sum)
  }
}