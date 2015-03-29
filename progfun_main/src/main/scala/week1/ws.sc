import scala.annotation.tailrec
def abs(x:Double) = if (x < 0) -x else x
def sqrt(x:Double) : Double = {
  def improve(guess: Double): Double =
    (guess + x / guess) / 2
  def isGoodEnough(guess: Double): Boolean ={
    abs(guess * guess - x)/x < 0.001
  }
  @tailrec
  def sqrtIter(guess:Double): Double ={
    if(isGoodEnough(guess)) guess
    else sqrtIter(improve(guess))
  }
  sqrtIter(1)
}

def factorial(n:Int):Int ={
  @tailrec
  def loop(acc:Int,next:Int):Int =
    if(next == 0) acc
    else loop(acc * next, next-1)
  loop(n,n - 1)
}


factorial(5)
val a = sqrt(0.001)
val b = sqrt(0.1e-20)
val c = sqrt(1e20)
val c2 = sqrt(1e30)
val c3 = sqrt(1e40)
val d = sqrt(1e60)
