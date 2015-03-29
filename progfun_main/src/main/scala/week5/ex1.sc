def init[T](l:List[T]):List[T] = l match {
  case Nil => throw  new NoSuchElementException
  case x::Nil => Nil
  case x::xs => x::init(xs)
}



def removeAt[A](n : Int, xs:List[A]) : List[A] = xs match {
  case Nil => Nil
  case h::t if n ==0 => t
  case h :: t => h :: removeAt(n-1,t)
}

def flatten[Any](l:List[Any]):List[Any] = {
  l match {
    case Nil => Nil
    case (h:List[Any]) :: xs => flatten(h)  ::: flatten(xs)
    case h ::xs => h :: flatten(xs)
  }
}
def merge(xs : List[Int], ys:List[Int]) :List[Int] = (xs,ys) match {
  case (Nil,ys) => ys
  case (xs,Nil) => xs
  case (x::xs1, y::ys1) =>
    if (x < y)
      x::merge(xs1,ys)
    else y::merge(xs,ys1)
}
def msort(xs:List[Int]) :List[Int] = {
  val  n = xs.length /2
  if(n == 0) xs
  else{
    val (ys,zs) = xs splitAt n
    merge(msort(ys),msort(zs))
  }
}


msort(List(5,2,7,1,0,80))


flatten(List(1,1,List(2,List(3, List(5,8)))))

removeAt(1,List('a','b','c','d'))