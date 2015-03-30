
def getNextPermutation(list: Vector[Int]): Vector[Int] = {
  val zip: Vector[(Int, Int)] = list zip (list.tail :+ -1)
  val (l, r) = lastSpan(zip, (x: (Int, Int)) => x._1 > x._2)
  val tuple1: (Vector[Int], Vector[Int]) = l unzip
  val (left, r1) = tuple1
  val tuple: (Vector[Int], Vector[Int]) = r unzip
  val (right, r2) = tuple
  val rev = right.reverse
  (left.init :+ rev.head :+ left.last) ++ rev.tail
}




def lastSpan(list: Vector[(Int, Int)], p: ((Int, Int)) => Boolean): (Vector[(Int, Int)], Vector[(Int, Int)]) = {
  def req(xs: Vector[(Int, Int)], int: Int): Int = {
    if (p(xs.last)) req(xs.init, int - 1)
    else int
  }
  val req1: Int = req(list, list.size)
  list.splitAt(req1)
}

//0231->0312
val list =  Vector(3,2,0,1)
val zip: Vector[(Int, Int)] = list zip (list.tail :+ -1)
val (l, r) = lastSpan(zip, (x: (Int, Int)) => x._1 > x._2)
val left = (l unzip)._1
val right = (r unzip)._1
val rev = right.reverse
val ind = rev.indexWhere(x=>x>=left.last + 1)
(left.init:+rev(ind))++rev.updated(ind,left.last)
//getNextPermutation(list)
/*
def permutationStream(init: Vector[Int], stream: Stream[Vector[Int]]): Stream[Vector[Int]] = {
  val permutation: Vector[Int] = getNextPermutation(init)
  init #:: permutationStream(permutation, permutation #:: stream)
}
/*init #:: permutationStream(getNextPermutation)
permutationStream(getNextPermutation(stream.head) #:: stream)*/

val streamPerm = permutationStream(Vector(0, 1, 2, 3, 4, 5, 6, 7, 8 , 9),Stream())

val toVector: Vector[Vector[Int]] = streamPerm.take(2224).toVector
toVector.map(x=>x.mkString).mkString("\n")
//Vector(0, 1, 3, 4, 2)*/
