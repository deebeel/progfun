class Rational(x:Int,y:Int){

  private def gcd(a:Int,b:Int):Int = if( b== 0) a else gcd(b,a % b)
  private lazy val g = gcd(numer,denumer)
  val numer = x
  val denumer = y
  def neg =
    new Rational(-numer,denumer)

  def sum(that:Rational) =
    new Rational(numer * that.denumer + that.numer * denumer,that.denumer * denumer)

  def sub(that:Rational) =sum(that.neg)

  override def toString = numer/g + "/" +denumer/g
}


val a = new Rational(4,2)
val b = new Rational(5,7)
val c = new Rational(3,2)


a.sub(b).sub(c)