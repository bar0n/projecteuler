Stream.from(0)
def str(stream: Stream[Int]): Stream[Int] = (999999999 - stream.head) #:: str(stream.tail)
str(Stream.from(0)).take(100).toList
def isPandigital(int: Int): Boolean = {
  val str = int.toString
  val seq = 2 to 9
  val segmentsLength = seq.map(x => str.length / x)

  def divide(int: Int): Vector[Int] = {
    val base = Integer.parseInt(str.take(int))
    def divide_(input: String, arr: Vector[String]): Vector[String] = {
      val nextLength = (base * (arr.length + 1)).toString.length
      if (input.length / nextLength <= 1) arr :+ input
      else {
        val (string1, string2) = input.splitAt(nextLength)
        divide_(string2, arr :+ string1)
      }
    }
    divide_(str, Vector()).map(x => Integer.parseInt(x))
  }

  val segments = segmentsLength.map(divide)

  def isPandigital(vect: Vector[Int]): Boolean = (1 to vect.length).map(x => x * vect.head) == vect

  segments.exists(x => isPandigital(x))
}
str(Stream.from(0)).filter(isPandigital).take(1).toList
isPandigital(192384576)
isPandigital(918273645)
isPandigital(999919999)
/*
9999*2
val int = 999919999
val str = int.toString
val seq = 2 to 9
val segmentsLength = seq.map(x => str.length / x)

def divide(int: Int): Vector[Int] = {
  val base = Integer.parseInt(str.take(int))
  def divide_(input: String, arr: Vector[String]): Vector[String] = {
    val nextLength = (base * (arr.length + 1)).toString.length
    if (input.length / nextLength <= 1) arr :+ input
    else {
      val (string1, string2) = input.splitAt(nextLength)
      divide_(string2, arr :+ string1)
    }
  }
  divide_(str, Vector()).map(x => Integer.parseInt(x))
}

val segments = segmentsLength.map(divide)

def isPandigital(vect: Vector[Int]): Boolean = {
  (1 to vect.length).map(x => x * vect.head) == vect
}
segments zip segments.map(x => isPandigital(x))
isPandigital(Vector(9999, 19999))
val vect = Vector(9999, 19999)
((1 to vect.length) zip vect).map({ case (x, y) => y / x })
(1 to vect.length).map(x => x * vect.head) == vect */
