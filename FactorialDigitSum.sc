def factorial(int: Int, prod: BigInt): BigInt = {
  if (int == 0) prod
  else factorial(int - 1, prod * int)
}

factorial(100,1).toString().map(x=>Integer.parseInt(x.toString)).sum

