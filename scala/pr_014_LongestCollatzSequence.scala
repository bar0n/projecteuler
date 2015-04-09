
import scala.collection.immutable.TreeMap
import scala.collection.mutable.HashMap

/**
 * Created by dbaron on 22.01.15.
 */
object pr_014_LongestCollatzSequence {
  var map: HashMap[Int, Int] = HashMap()
  var map2: HashMap[Int, Int] = HashMap()
  var mapMy: TreeMap[Long, Int] = TreeMap()


  def collazt(n: Int, sizePath: Int): Int = {


    if (map.contains(n) || n == 1) {
      val value: Int = map.getOrElse(n, 1)
      map.+=(n -> value)
      value + sizePath - 1
    }
    else if (n % 2 == 0) {
      val collazt1: Int = collazt(n / 2, sizePath + 1)
      map.+=(n / 2 -> (collazt1 - sizePath))
      collazt1
    }
    else {
      val collazt2: Int = collazt(3 * n + 1, sizePath + 1)
      map.+=(3 * n + 1 -> (collazt2 - sizePath))
      collazt2
    }
  }

  def put(key: Int, value: Int) {
    map2.put(key, value)
    if (key == 14) {
      println(key + " " + value)
    }
  }

  def collazt2(n: Int): Int = {

    var m = n
    var sizePath = 0
    var list = List(1 -> 0)
    while (m > 1) {
      //print("->"+m)
      val maybeInt: Option[Int] = map2.get(m)
      if (maybeInt.nonEmpty) {
        list = list.map(x => (x._1, sizePath - x._2 + maybeInt.get + 1))
        m = 1
        sizePath = sizePath + maybeInt.get
      } else {
        sizePath = sizePath + 1
        list = list :+ (m -> sizePath)
        if (m % 2 == 0) {
          m = m / 2
        } else {
          m = 3 * m + 1
        }
      }
    }
    list.foreach({
      case (i, j) =>
        if (i > 1) put(i, sizePath - j + 1
        )
    })
    put(n, sizePath)
    // println("->1 ")
    sizePath

  }

  def collazt3(n: Int): Int = {
    var m:Long = n
    var sizePath = 0

    while (m > 1) {
      // print("->" + m)
      sizePath = sizePath + 1
      if (m % 2 == 0) {
        m = m / 2
      } else {
        m = 3 * m + 1
      }

    }
    //  println("->1 ")
    sizePath
  }

  def collaztNew(n: Int): Int = {
    var m:Long = n
    var sizePath = 0
    var list: List[Long] = List()
    while (m > 1) {
      if (mapMy.contains(m)) {
        sizePath = sizePath + mapMy.getOrElse(m, 0)
        m = 1
      } else {
        list = list :+ m
        sizePath = sizePath + 1
        if (m % 2 == 0) {
          m = m / 2
        } else {
          m = 3 * m + 1
        }
      }
    }
    mapMy = mapMy ++ (list zip (sizePath to 1 by -1))
    mapMy.getOrElse(n, 0)
  }

  def seqCollazt(value: Int): Int = {
    val path = collazt(value, 0) + 1
    map.+=(value -> path)
    // println(value+" : "+map)
    path
  }


  def main(args: Array[String]) {

/*
    2015-01-27T10:37:24.518+02:00
    475
    2015-01-27T10:38:10.976+02:00
    Some((910107,475))
*/

/*    println(collazt3(910107))
    println(collazt3(837799))*/

    val seq = 1000000 to 13 by -1
    //println(new DateTime())
    TreeMap((seq zip seq.map(collaztNew)).toSeq: _*)
    val mx = mapMy.values.max
    println(mx)
    val filter = mapMy.find({ case (k, v) => v == mx})
    //println(new DateTime())
    println(filter)

  }
}
