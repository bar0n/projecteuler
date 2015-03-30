val root: PartialFunction[Double,Double] = {
  case d if d >= 0 => math.sqrt(d)
}
root.isDefinedAt(-1)

List(0.5, -0.2, 4).collect(root)

def add(i: Int, j: Int) = i + j
val add5 = add(_: Int,5)
add5(2)
val addTupled = (add _).tupled

List((1,2), (4,5), (3,8)).map(addTupled)
val addCurried = (add _).curried
val cur = List(1,4,3).map(addCurried)

cur.head(3)