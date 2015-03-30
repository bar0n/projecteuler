import scala.collection.immutable
val N = 21
val arr = Array.ofDim[Long](N,N)
/*val longs: Array[Array[Int]] = Array.ofDim[Int](20,20)
longs.map(_.mkString(",")).mkString("\n")*/
for {
  i <- 0 until N
} {
  arr(i)(0) = i
  arr(0)(i) = i

    arr(i)(1) = i + 1
    arr(1)(i) = i + 1

}
for {
  i <- 2 until N
  j <- 2 until N
} arr(i)(j) = arr(i-1)(j)+arr(i)(j-1)
"\n"+arr.map(_.mkString(",")).mkString("\n")
