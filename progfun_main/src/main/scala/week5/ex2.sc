

def pack[T](xs: List[T]): List[List[T]] ={
  def loop[T](acc:List[List[T]],xs:List[T]) : List[List[T]] = xs match{
    case Nil => acc
    case y::ys =>
      val (a,b) = xs.partition(_==y)
      loop( a :: acc,b)
  }
  loop(Nil,xs)
}






def pack2[T](xs: List[T]): List[List[T]] = xs match{
  case Nil => Nil
  case y::ys =>
    val (first,rest) = xs.span((x) => x == y)
    first :: pack2(rest)
}

pack(List(1,2,1,2,2))


def encode[T](xs:List[T]):List[(T,Int)] = pack(xs) map (x => (x.head, x.length))

encode(List(1,2,2,2,4,5,1,4,5))

def lengthFun[T](xs: List[T]): Int =
  (xs foldRight 0)((x,y) => y + 1 )

lengthFun(List(1,2,3,5,6,6,7))

def mapFun[T, U](xs: List[T], f: T => U): List[U] =
  (xs foldRight List[U]())(f(_)::_)


mapFun(List(1,2,3,5,6,6,7),(x:Int)=>(x * x))