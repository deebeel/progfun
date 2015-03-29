def isPrime(n:Int):Boolean = n match {
  case 0 => false
  case 1 => true
  case x => (2 until n) forall (n % _ != 0)
}


def scalarProduct(xs:List[Double], ys:List[Double]) : Double =
  (for{
    (x,y) <- xs zip ys
  }yield x * y).sum





def queens(n:Int) :Set[List[Int]] ={

  def isSafe(col: Int, queens: List[Int]) = {
    val row = queens.length
    val placedQueens = (row - 1 to 0 by -1) zip queens
    placedQueens forall {
      case (r,c) => c != col && math.abs(col - c) != row -r
    }
  }
  def placeQueens(k:Int):Set[List[Int]] = {
    if( k == 0) Set(List())
    else
      for{
        queens <- placeQueens(k - 1)
        col <-  0 until n
        if isSafe(col,queens)
      } yield col :: queens
  }
  placeQueens(n)
}





