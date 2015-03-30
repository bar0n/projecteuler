def isPalindromStr(str: String): Boolean = {
  val half = str.length / 2
  str.take(half) == str.takeRight(half).reverse
}
def isPalindrom(num: Int): Boolean = {
  isPalindromStr(num.toString)
}
def isBynPalindrom(int:Int):Boolean={
  isPalindrom(int) && isPalindromStr(Integer.toBinaryString(int))
}
isBynPalindrom(585)

val seq = 1 to 1000000

seq.filter(isBynPalindrom).sum

