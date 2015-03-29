
trait IntSet {

}

class Empty extends  IntSet{}
class  NonEmpty extends IntSet{}

trait List[+E]{
  def isEmpty:Boolean
  def head:E
  def tail:List[E]
  def prepend[U >: E](elem:U):List[U] = new Cons(elem,this)
}

class Cons[E](val head:E, val tail:List[E]) extends  List[E]{
  val isEmpty: Boolean = false
  override def toString: String = head.toString + "," +tail.toString
}
object Nil extends  List[Nothing]{
  val isEmpty:Boolean = true

  def head: Nothing =  throw new NoSuchElementException

  def tail: Nothing = throw new NoSuchElementException
  override def toString = "NIL"
}
object List{
  def f(xs :List[NonEmpty], x:Empty) =xs prepend x
  def apply[E](args:E*):List[E]={
    if(args.length == 0) Nil
    else new Cons(args.head,apply(args.tail:_*))
  }
}

List(1,3,4,5,6)
val a = List.f(new Cons(new NonEmpty,Nil),new Empty)
a.toString



