val map = Map(2 -> 2, 3 -> 3, 4 -> 5, 5 -> 7, 6 -> 11, 7 -> 13, 8 -> 17)

(0 to 9 permutations).toStream
val s = 2 to 8
//val vect = Vector(1, 4, 0, 6, 3, 5, 7, 2, 8, 9)

def isProperty(vect: IndexedSeq[Int]): Boolean = s zip s.map(x => Integer.parseInt(vect.drop(x - 1).take(3).mkString(""))) forall { case (i, x) => x % map(i) == 0 }

//(0 to 9 permutations).filter(x=>x)

//(0 to 9 permutations).toStream.filter(isProperty).map(x=>Integer.parseInt(x.mkString(""))).take(4).toList

Integer.parseInt("106357289")

//Vector(1406357289, 1430952867, 1460357289, 4106357289, 4130952867, 4160357289).sum