def toLong(int:Int):Long = {
  val l:Long = int
  l
}

def sum_2(n:Long):Long = 16*n*n+4*n+4
def sum(n:Int):Long = ((1 to n) map toLong map sum_2).sum + 1
1 to 1001 map( x=>2*(x+1)-1) zip (1 to 1001 map sum) filter {case (x,y) => x==1001}
/*def f_ru(n:Long):Long = (2*n+1)*(2*n+1)
def f_rd(n:Long):Long = (2*(n-1)+1)*(2*(n-1)+1)+2*n
def f_ld(n:Long):Long = 4*n*n+1
def f_lu(n:Long):Long = 4*n*n+1+2*n
def sum_1(n:Long):Long = f_ru(n)+ f_rd(n)+f_ld(n)+f_lu(n)*/


/*1 to 7 zip (1 to 7 map f_ru)
1 to 7 zip (1 to 7 map f_rd)
1 to 7 zip (1 to 7 map f_ld)
1 to 7 zip (1 to 7 map f_lu)
1 to 7 zip (1 to 7 map sum_1)*/

/*
f_rd(1)
f_ru(1)*/
