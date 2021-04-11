package smtwrap

// quick and dirty implementation with weak typing, for instance not recognizing boolean expressions.
trait SMTExpr extends IntOps with BoolOps {
  val view: String
}

case class IntSMTExpr(view: String) extends SMTExpr

case class BoolSMTExpr(view: String) extends SMTExpr

trait SMTOps {
  val view: String

  def op[A: SMTView](opName: String, that: A) =
    s"($opName $view ${implicitly[SMTView[A]].smtView(that)})"

}

trait IntOps extends SMTOps {
  def =:=[A : IntSMTView](that: A) = IntSMTExpr(op("=", that))

  def +[A : IntSMTView](that: A) = IntSMTExpr(op("+", that))

  def -[A : IntSMTView](that: A) = IntSMTExpr(op("-", that))

  def *[A : IntSMTView](that: A) = IntSMTExpr(op("*", that))

  def <[A : IntSMTView](that: A) = IntSMTExpr(op("<", that))

  def >[A : IntSMTView](that: A) = IntSMTExpr(op(">", that))

  def <=[A : IntSMTView](that: A) = IntSMTExpr(op("<=", that))

  def >=[A : IntSMTView](that: A) = IntSMTExpr(op(">=", that))

  def unary_- = IntSMTExpr(s"(- $view)")
}

trait BoolOps extends SMTOps {
  def assert = BoolSMTExpr(s"(assert $view)")
}

object SMTExpr {
  def double(name: Double) = IntSMTExpr(name.toString()) // FIXME: quick fix

  def int(name: Int) = IntSMTExpr(name.toString())

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

trait BoolSMTView[A] extends SMTView[A]

object SMTView {

  implicit val intSmtWithView: IntSMTView[IntSMTExpr] =
    new IntSMTView[IntSMTExpr] {
      def smtView(a: IntSMTExpr): String = a.view
    }

  implicit val boolSmtWithView: BoolSMTView[BoolSMTExpr] =
    new BoolSMTView[BoolSMTExpr] {
      def smtView(a: BoolSMTExpr): String = a.view
    }

  implicit val intSMT: IntSMTView[Int] = new IntSMTView[Int] {
    def smtView(a: Int): String = a.toString()
  }

  implicit val doubleSMT: SMTView[Double] = new SMTView[Double] {
    def smtView(a: Double): String = a.toString()
  }
}
