case class Node(int: Int, left: Node, right: Node)

/*

3
7 4
2 4 6
8 5 9 3*/
type Pair = (Int, Int)
val list = List(
  List(75),
  List(95, 64),
  List(17, 47, 82),
  List(18, 35, 87, 10),
  List(20, 4, 82, 47, 65),
  List(19, 1, 23, 75, 3, 34),
  List(88, 2, 77, 73, 7, 63, 67),
  List(99, 65, 4, 28, 6, 16, 70, 92),
  List(41, 41, 26, 56, 83, 40, 80, 70, 33),
  List(41, 48, 72, 33, 47, 32, 37, 16, 94, 29),
  List(53, 71, 44, 65, 25, 43, 91, 52, 97, 51, 14),
  List(70, 11, 33, 28, 77, 73, 17, 78, 39, 68, 17, 57),
  List(91, 71, 52, 38, 17, 14, 91, 43, 58, 50, 27, 29, 48),
  List(63, 66, 4, 68, 89, 53, 67, 30, 73, 16, 69, 87, 40, 31),
  List(4, 62, 98, 27, 23, 9, 70, 98, 73, 93, 38, 53, 60, 4, 23)
)

//val list = List(List(3), List(7, 4), List(2, 4, 6), List(8, 5, 9, 3))


def coord(pair: Pair): Int = list(pair._1)(pair._2)
def neighbors(pair: Pair): List[Pair] = {
  val (i, j) = pair
  if (i + 1 < list.size)
    List(i + 1 -> j, i + 1 -> (j + 1))
  else List()
}

//def cond()

def from(paths: List[Pair], sum: List[List[Pair]]): List[List[Pair]] = {

  val neighbors1: List[(Int, Int)] = neighbors(paths.last)
  if (neighbors1.isEmpty) sum
  else neighbors1.flatMap(x => from(paths :+ x,
    if (neighbors(x).isEmpty) sum :+ (paths :+ x)
    else sum))
}
val from1 = from(List(0 -> 0), Nil)
val sums = from1.map(path => path.map(y => coord(y)).sum)
from1(sums.indexOf(sums.max))
val maxPath = from1(sums.indexOf(sums.max)).map(coord)
maxPath.sum

