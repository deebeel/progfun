import scala.annotation.tailrec

trait Expr
case class Number( x:Int) extends Expr
case class Val( x:String)  extends Expr
case class Sum( left:Expr, right:Expr) extends Expr
case class Prod( left:Expr, right:Expr) extends Expr
object Expr{
  def show(e:Expr):String = e match {
    case Number(x) => x.toString
    case Val(x) => x
    case Sum(x,y) => show(x) + " + "+ show(y)
    case Prod(x:Expr,y:Expr) => {x match {
      case Sum(_,_) => "(" + show(x) + ")"
      case _ => show(x)
    }} + " * " + {y match {
      case Sum(_,_) => "(" + show(y) + ")"
      case _ => show(y)
    }}

  }
}

def insert(head:Int,tail:List[Int]):List[Int] ={
  tail match{
    case Nil => List(head)
    case x::xs => if(head > x) x::insert(head,xs)
      else head::tail
  }
}

def isort(xs:List[Int]):List[Int] = xs match{
  case Nil => xs
  case head::tail => insert(head,isort(tail))
}


val list: List[Int] = List(4, 6, 2, 90, 1, 5)
isort(list)
