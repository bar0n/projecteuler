/**
 * Created by dbaron 
 */
object SubStringDivisibility {
  def main(args: Array[String]) {
    val map = Map(2 -> 2, 3 -> 3, 4 -> 5, 5 -> 7, 6 -> 11, 7 -> 13, 8 -> 17)

    (0 to 9 permutations).toStream
    val s = 2 to 8
    //val vect = Vector(1, 4, 0, 6, 3, 5, 7, 2, 8, 9)

    def isProperty(vect: IndexedSeq[Int]): Boolean = s zip s.map(x => Integer.parseInt(vect.drop(x - 1).take(3).mkString(""))) forall { case (i, x) => x % map(i) == 0 }

    //(0 to 9 permutations).filter(x=>x)

    val dd = (0 to 9 permutations).toStream.filter(isProperty).map(x=>java.lang.Long.parseLong(x.mkString(""))).take(100).toList

    println(dd)
    println(dd.sum)
  }
}
