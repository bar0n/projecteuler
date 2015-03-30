def power(N:BigInt,pow:Int,bigInt: BigInt):BigInt=
if (pow==0) bigInt
else
  if (pow%2==0) power(N*N,pow/2,bigInt)
  else power(N,pow-1,N*bigInt)

val  pow= power(2,1000,BigInt(1))

pow.toString().map(x => Integer.parseInt(x.toString)).sum