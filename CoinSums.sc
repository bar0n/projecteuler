val coins = Vector(200, 100, 50, 20, 10, 5, 2, 1)
val n = 200

val v: Vector[(Int, Int)] = coins map (x => x -> n / x)

val count = n / coins.head

def comb(v: Vector[Int], sum: Int): Int = {
  if (sum == 0) 1
  else if (v.isEmpty || sum < 0) 0
  else
    comb(v.tail, sum) + comb(v, sum - v.head)
}

comb(coins, n)
