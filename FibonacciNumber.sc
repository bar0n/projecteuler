

type MTX = ((BigInt, BigInt), (BigInt, BigInt))
def p(m: MTX, k: MTX): MTX = {
  val ((a1, b1),
  (c1, d1)) = m
  val ((a2, b2),
  (c2, d2)) = k
  ((a1 * a2 + b1 * c2, a1 * b2 + b1 * d2),
    (b1 * a2 + d1 * c2, b1 * b2 + d1 * d2))
}
def power(a: MTX, n: Int) = pow(a, a, n - 1)
def lenth(d: MTX): Boolean = {
  val ((_, b1), (_, _)) = d
  b1.toString().size >= 1000
}

def lenth2(d: MTX): Boolean = {
  val ((_, b1), (_, _)) = d
  println("b1.toString().size"+b1.toString().size)
  b1.toString().size >= 1000
}


def pow(a: MTX, prod: MTX, n: Int): MTX = {
  /*if (lenth(prod)) {
    println("n="+n)
    prod
  }
  else */if (n == 0) prod
  //else if (n % 2 == 0) pow(p(a, a), prod, n / 2)
  else pow(a, p(a, prod), n - 1)
}
val init = ((BigInt(0), BigInt(1)), (BigInt(1), BigInt(1)))
power(init, 6)
def rev(mtx: MTX): MTX = {
  println(mtx)
  if (lenth2(mtx)) {
    val ((a1, b1),
    (c1, d1)) = mtx
    rev(((b1 - a1, d1 - c1), (a1, c1)))
  }
  else mtx
}
200000 - 195218
val power1: ((BigInt, BigInt), (BigInt, BigInt)) = power(init, 4781)
val ((a1, b1),
(c1, d1)) = power1
val result = d1
a1.toString().size
b1.toString().size
c1.toString().size
d1.toString().size
