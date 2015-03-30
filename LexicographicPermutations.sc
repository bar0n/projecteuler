def permutation2(list: List[Int]): List[List[Any]] = {
  def switch(xs: List[Int]): List[List[Int]] = {
    xs match {
      case head::Nil => List(xs)
      case _ =>   val seq = 1 until xs.size
        xs::seq.map(x => {
          val (first, second) = xs.splitAt(x)
          second.head :: first ++ second.tail
        }).toList
    }
  }
  list match {
    case Nil => List(List())
    case lst =>  for {
      element <- switch(lst)
      next <- permutation2(element.tail)
    } yield element.head :: next
  }
}
val perm = permutation2(List(0, 1, 2, 3,4)).map(x=>x.mkString).mkString("\n")
def getNextPermutation(list:List[Int]):List[Int]={
  list zip (0 :: list.tail)
  list
}
//val list  = List(0,4,3,2,1)
val list  = List(1,0,4,3,2).reverse
val zip= list zip (list.tail :+ 0)
def cond(int:Int,int2:Int):Boolean= {
  println(""+int+" "+int2+" "+(int>int2))
  int>int2
}
zip.span(x=>cond(x._1,x._2))
/*def cond(list: List[Int],xs: List[Int]):Boolean={
  println("element = "+list +" next = "+xs)
  true
}*/
//val list = List(List(0, 1, 2))
/*def permutation(list: List[List[Int]]): List[List[Int]] = {
  val seq = 1 until list.size
  list.map(xs=> {
    seq.map(x => {
      val (first, second) = xs.splitAt(x)
      second.head :: first ++ second.tail
    })
  }
}*/
/*
def permutation(list: List[List[Int]]): List[List[Int]] = {
  list match {
    case Nil => list
    case _ =>
      val seq = 1 until list.head.size
      val switched = list.flatMap(xs => {
        seq.map(x => {
          val (first, second) = xs.splitAt(x)
          second.head :: first ++ second.tail
        })
      })
      switched ++ switched.flatMap(xs => permutation(List(xs.tail)).map(x => xs.head :: x))
  }
}
permutation(list)
val cx = List(List(1, 0, 2), List(2, 0, 1))
cx.flatMap(xs => permutation(List(xs.tail)).map(x => xs.head :: x))
*/
