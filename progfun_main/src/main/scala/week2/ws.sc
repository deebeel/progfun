def product(f:Int => Int)(a:Int,b:Int):Int =
  if(a > b) 1
  else f(a) * product(f)(a + 1,b)


def fact(a:Int):Int =
  product(x => x)(1,a)



def gen(f:(Int,Int) => Int,unit:Int)(g:Int => Int)(a:Int,b:Int):Int =
  if(a > b) unit
  else f(a,gen(f,unit)(g)(a+1,b))



def isCloseEnough(guess: Double,x:Double): Boolean =
  Math.abs((x-guess)/x)/x < 0.001

def fixedPoint(f:Double => Double)
              (firstGuess:Double)
              :Double = {
  def iter(guess:Double):Double={
    val next = f(guess)
    if(isCloseEnough(guess,next)) next
    else iter(next)
  }
  iter(firstGuess)
}


def averageDamp(f:Double=>Double)(x:Double) = (x + f(x))/2



def sqrt(x:Double):Double =
  fixedPoint(averageDamp(y => x/y))(1)