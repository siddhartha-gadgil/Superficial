package smtwrap

// quick and dirty implementation with weak typing, for instance not recognizing boolean expressions.
case class SMTExpr(view: String) extends IntOps with BoolOps

trait SMTOps {
  val view: String

  def op[A: SMTView](opName: String, that: A) =
    s"($opName $view ${implicitly[SMTView[A]].smtView(that)})"

  def declare(tp: String) = SMTExpr(s"(declare-fun $view() $tp)")
}

trait IntOps extends SMTOps {
  def =:=[A: SMTView](that: A) = SMTExpr(op("=", that))

  def +[A: SMTView](that: A) = SMTExpr(op("+", that))

  def -[A: SMTView](that: A) = SMTExpr(op("-", that))

  def *[A: SMTView](that: A) = SMTExpr(op("*", that))

  def <[A: SMTView](that: A) = SMTExpr(op("<", that))

  def >[A: SMTView](that: A) = SMTExpr(op(">", that))

  def <=[A: SMTView](that: A) = SMTExpr(op("<=", that))

  def >=[A: SMTView](that: A) = SMTExpr(op(">=", that))

  def unary_- = SMTExpr(s"(- $view)")
}

trait BoolOps extends SMTOps {
  def assert = SMTExpr(s"(assert $view)")
}

object SMTExpr {
  def double(name: Double) = SMTExpr(name.toString())

  def int(name: Int) = SMTExpr(name.toString())

  def declReal(name: String): String = s"(declare-fun $name() Real)"

  def declInt(name: String): String = s"(declare-fun $name() Int)"
  implicit class IntSMT(n: Int) extends IntOps {
    val view: String = n.toString()
  }

  implicit class DoubleSMT(n: Double) extends SMTOps {
    val view: String = n.toString()
  }
}

trait SMTView[A] {
  def smtView(a: A): String
}

trait IntSMTView[A] extends SMTView[A]

object SMTView {
  implicit val smtWithView: SMTView[SMTExpr] = new SMTView[SMTExpr] {
    def smtView(a: SMTExpr): String = a.view
  }

  implicit val intSMT: IntSMTView[Int] = new IntSMTView[Int] {
    def smtView(a: Int): String = a.toString()
  }

  implicit val doubleSMT: SMTView[Double] = new SMTView[Double] {
    def smtView(a: Double): String = a.toString()
  }
}
