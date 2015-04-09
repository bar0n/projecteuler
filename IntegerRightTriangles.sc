val seq = for {
  a <- 1 to 999
  b <- 1 to 999
  (c: Int) = isSrqt(a, b)
  if c * c == a * a + b * b && (a + b + c < 1000)
} yield (a, b, c)

def isSrqt(a: Int, b: Int): Int = {
  val x = a * a + b * b
  val c = math.sqrt(x).toInt
  c
}

val www = seq.groupBy({ case (a, b, c) => a + b + c }).map({ case (c, x) => (c, x.size) }).toSeq.sortBy({case(x,y)=>y})