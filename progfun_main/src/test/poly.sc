
class Poly(terms0:Map[Int,Double]){
  def this(pairs:(Int,Double)*) = this(pairs.toMap)
  val terms = terms0 withDefaultValue 0.0

  def + (other:Poly) =
    new Poly((other.terms foldLeft terms)(addTerm))

  private def addTerm(terms:Map[Int,Double], term:(Int,Double)) = {
    val (exp,coeff) = term
    terms  updated (exp ,coeff + terms(exp))
  }

  override def toString: String =
    (for((exp,coeff) <- terms) yield (if (coeff >= 0) "+" else "" ) + coeff + "x^" + exp) mkString
}



val a = new Poly(1 -> 2, 3-> 4, 5 -> 6)
val b = new Poly(1 -> 3, 3-> -44, 5 -> 6)
a + b