import scala.io.Source

val word = Source.fromFile("/home/dbaron/java/projects/tms2/projecteuler/words.txt").getLines().toList.map(x => x.replace("\"", ""))
val words = word.head.split(",").toVector
def wordSum(str: String): Int = str.map(x => x.toInt - 64).sum
val wordSums = words.map(wordSum)
val max = wordSums.max
def triangleNumbers(n: Int) = n * (n + 1) / 2
val triangles = Stream.from(1).map(triangleNumbers).takeWhile(x => x <= max).toVector

wordSums.count(x=>triangles.contains(x))

