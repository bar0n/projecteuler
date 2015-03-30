def fract(n: Int, d: Int) = {
  val n1 = n / 10
  val n2 = n % 10
  val d1 = d / 10
  val d2 = d % 10
  (n * d2 == n1 * d || n * d1 == n2 * d) && (n1!=n2 && d1!=d2 && (n1==d2 || n2==d1))
}
49 / 10
49 % 10
val n = 49
val d = 98
fract(n, d)
fract(30, 50)
val fr = for {
  num<-10 to 99
  den<-10 to 99
  if fract(num,den) && num<den
} yield (num,den)

def gcd(a: Int, b: Int): Int = {
  if (b == 0) a else gcd(b, a % b)
}

val numer = fr.map({ case ( dn,_) => dn}).product
val denumer = fr.map({ case (_, dn) => dn}).product

denumer/gcd(numer,denumer)







