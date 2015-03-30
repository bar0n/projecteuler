/**
 * Created by dbaron on 20.01.15.
 */
object P10001StPrime {
  def main(arg: Array[String]): Unit = {

    def prime(stream: Stream[Int]): Stream[Int] = stream.head #:: prime(stream.tail.filter(x => x % stream.head != 0))
    println(prime(Stream.from(2)).take(6).last)
    println(prime(Stream.from(2)).take(10001).last)
  }
}
