package smtsolve

sealed trait SExpression {
  val view: String
}

object SExpression {
  case class Atom(view: String) extends SExpression

  case class Parens(terms: Vector[SExpression]) extends SExpression {
    val view: String = s"(${terms.map(_.view).mkString(" ")})"
  }

  import fastparse._, NoWhitespace._

  val separators = Vector("(", ")", " ", "\n", "\t")

  def token[_: P]: P[SExpression] =
    CharPred(c => !separators.contains(c.toString)).rep(1).!.map(Atom(_))

  def parens[_: P]: P[SExpression] =
    P(("(" ~ sexpr ~
     (CharIn(" \n\t").rep(1) ~ sexpr).rep ~ ")").map {
      case (head, tail) => Parens(head +: tail.toVector)
    })

  def sexpr[_: P]: P[SExpression] = P(token | parens)

  def get(s: String): SExpression =
    parse(s, sexpr(_)).fold({
      case (message, _, extra) =>
        throw new Exception("Could not parse " + s + " with error" + message)
    }, { case (exp, _) => exp })

  def getMap(s: String) = get(s) match {
      case Atom(view) => throw new Exception("atom $view is not a map")
      case Parens(terms) => terms.collect{
          case Parens(Vector(key, value))  => key.view -> value.view 
          case exp => throw new Exception(s"$exp is not a key-value s-expression") 
      }.toMap
  }
}
