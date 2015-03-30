def power(osn: BigInt, step: BigInt, pow: BigInt): BigInt =
  if (step == 0) pow
  else if (step % 2 == 0) power(osn * osn, step / 2, pow)
  else power(osn, step - 1, pow * osn)
def f(a: Int, b: Int, c: Int, d: Int): Boolean =
  power(a, b, BigInt(1)) == power(c, d, BigInt(1))
val n = 60
val seq = 2 to n
val d = (for {
  a <- seq
  b <- seq
  c <- seq
  d <- seq
  if a < c && d < b && f(a, b, c, d)
} yield (a,b)).distinct
/*d.groupBy(x=>x).mapValues(_.size)
val sub = d.groupBy(x=>x).mapValues(_.size).map({case(_,x)=>if (x>1) x-1 else x }).sum
d.groupBy(identity).mapValues(_.size).toSeq*/
//(power(a, b , BigInt(1)),power(c,d,BigInt(1)))
(n - 1) * (n - 1)
(n - 1) * (n - 1) - d.size
val d2 = (for {
  a <- seq
  b <- seq
} yield power(a,b,BigInt(1))).sorted
d2.size
d2.sorted.groupBy(x=>x).mapValues(_.size).filter({case(_,x)=>x>1})
d2.distinct.size
