package smtwrap

// quick and dirty implementation with weak typing, for instance not recognizing boolean expressions.
case class SMTExpr(view: String) extends SMTOps

trait SMTOps {
  val view: String

  def op[A: SMTView](opName: String, that: A) =
    SMTExpr(s"($opName $view ${implicitly[SMTView[A]].smtView(that)})")

  def =:=[A: SMTView](that: A): SMTExpr = op("=", that)

  def +[A: SMTView](that: A): SMTExpr = op("+", that)

  def -[A: SMTView](that: A): SMTExpr = op("-", that)

  def *[A: SMTView](that: A): SMTExpr = op("*", that)

  def <[A: SMTView](that: A): SMTExpr = op("<", that)

  def >[A: SMTView](that: A): SMTExpr = op(">", that)

  def <=[A: SMTView](that: A): SMTExpr = op("<=", that)

  def >=[A: SMTView](that: A): SMTExpr = op(">=", that)

  def unary_- = SMTExpr(s"(- $view)")

  def assert = SMTExpr(s"(assert $view)")

  def declare(tp: String) = SMTExpr(s"(declare-fun $view() $tp)")
}

object SMTExpr {
  def double(name: Double): SMTExpr = SMTExpr(name.toString())

  def int(name: Int): SMTExpr = SMTExpr(name.toString())

  def declReal(name: String): String = s"(declare-fun $name() Real)"

  def declInt(name: String): String = s"(declare-fun $name() Int)"

  def varVec(name: String, l: Seq[Int]): Vector[SMTExpr] = l.toVector.map { j =>
    SMTExpr(s"${name}_$j")
  }
  implicit class IntSMT(n: Int) extends SMTOps {
    val view: String = n.toString()
  }

  implicit class DoubleSMT(n: Double) extends SMTOps {
    val view: String = n.toString()
  }
}

trait SMTView[A] {
  def smtView(a: A): String
}

object SMTView {
  implicit val smtWithView: SMTView[SMTExpr] = new SMTView[SMTExpr] {
    def smtView(a: SMTExpr): String = a.view
  }

  implicit val intSMT: SMTView[Int] = new SMTView[Int] {
    def smtView(a: Int): String = a.toString()
  }

  implicit val doubleSMT: SMTView[Double] = new SMTView[Double] {
    def smtView(a: Double): String = a.toString()
  }
}
