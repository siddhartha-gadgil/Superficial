package smtwrap

// quick and dirty implementation with weak typing, for instance not recognizing boolean expressions.
sealed trait SMTExpr{
  val view: String

  val sort: String

  lazy val declare = {
      require(!view.contains(" "), s"$view is not a variable")
      SMTCommand(s"(declare-fun $view() $sort)")}
}

case class IntSMTExpr(view: String) extends SMTExpr with IntOps{
    def toReal = RealSMTExpr(s"(to_real $view)")

    val sort: String = "Int"
}

case class BoolSMTExpr(view: String) extends SMTExpr with BoolOps{
    val sort: String = "Bool"
}

object BoolSMTExpr{
    def apply(bool: Boolean): BoolSMTExpr = BoolSMTExpr(bool.toString())
}

case class RealSMTExpr(view: String) extends SMTExpr with RealOps{
    val realView: String = view

    val sort: String = "Real"
}

case class SMTCommand(text: String)
trait SMTOps {
  val view: String

  def op[A: SMTView](opName: String, that: A) =
    s"($opName $view ${implicitly[SMTView[A]].smtView(that)})"

}

trait IntOps extends SMTOps {
  lazy val intView = view

  def intOp[A: IntSMTView](opName: String, that: A) =
    IntSMTExpr(s"($opName $intView ${implicitly[IntSMTView[A]].smtView(that)})")

  def intBoolOp[A: IntSMTView](opName: String, that: A) =
    BoolSMTExpr(s"($opName $intView ${implicitly[IntSMTView[A]].smtView(that)})")

  def =:=[A: IntSMTView](that: A) = intOp("=", that)

  def +[A: IntSMTView](that: A) = intOp("+", that)

  def -[A: IntSMTView](that: A) = intOp("-", that)

  def *[A: IntSMTView](that: A) = intOp("*", that)

  def <[A: IntSMTView](that: A) = intBoolOp("<", that)

  def >[A: IntSMTView](that: A) = intBoolOp(">", that)

  def <=[A: IntSMTView](that: A) = intBoolOp("<=", that)

  def >=[A: IntSMTView](that: A) = intBoolOp(">=", that)

  def unary_- = IntSMTExpr(s"(- $view)")
}

trait RealOps extends SMTOps {
  val realView: String

  def realOp[A: RealSMTView](opName: String, that: A) =
    RealSMTExpr(
      s"($opName $realView ${implicitly[RealSMTView[A]].smtView(that)})"
    )
  def realBoolOp[A: RealSMTView](opName: String, that: A) =
    BoolSMTExpr(
      s"($opName $realView ${implicitly[RealSMTView[A]].smtView(that)})"
    )

  def =:=[A: RealSMTView](that: A) = realOp("=", that)

  def +[A: RealSMTView](that: A) = realOp("+", that)

  def -[A: RealSMTView](that: A) = realOp("-", that)

  def *[A: RealSMTView](that: A) = realOp("*", that)

  def /[A: RealSMTView](that: A) = realOp("/", that)

  def <[A: RealSMTView](that: A) = realBoolOp("<", that)

  def >[A: RealSMTView](that: A) = realBoolOp(">", that)

  def <=[A: RealSMTView](that: A) = realBoolOp("<=", that)

  def >=[A: RealSMTView](that: A) = realBoolOp(">=", that)

  def unary_- = RealSMTExpr(s"(- $view)")
}

trait BoolOps extends SMTOps {
  def assert = SMTCommand(s"(assert $view)")

  def &&(that: BoolSMTExpr) = BoolSMTExpr(s"(and $view ${that.view})")

  def ||(that: BoolSMTExpr) = BoolSMTExpr(s"(or $view ${that.view})")

  def unary_! = BoolSMTExpr(s"(not $view)")
}

object SMTExpr {
  def double(name: Double) = IntSMTExpr(name.toString()) // FIXME: quick fix

  def int(name: Int) = IntSMTExpr(name.toString())

  def declReal(name: String): String = s"(declare-fun $name() Real)"

  def declInt(name: String): String = s"(declare-fun $name() Int)"
  implicit class IntSMT(n: Int) extends IntOps {
    lazy val view: String = n.toString()
  }

  implicit class DoubleSMT(n: Double) extends RealOps {
    lazy val view: String = n.toString()

    lazy val realView = view
  }
}

trait SMTView[A] {
  def smtView(a: A): String
}

trait IntSMTView[A] extends SMTView[A]

trait BoolSMTView[A] extends SMTView[A]

trait RealSMTView[A] extends SMTView[A]

object SMTView {

  implicit val intSmtWithView: IntSMTView[IntSMTExpr] =
    new IntSMTView[IntSMTExpr] {
      def smtView(a: IntSMTExpr): String = a.view
    }

  implicit val boolSmtWithView: BoolSMTView[BoolSMTExpr] =
    new BoolSMTView[BoolSMTExpr] {
      def smtView(a: BoolSMTExpr): String = a.view
    }

  implicit val realSmtWithView: RealSMTView[RealSMTExpr] =
    new RealSMTView[RealSMTExpr] {
      def smtView(a: RealSMTExpr): String = a.view
    }

  implicit val intSMT: IntSMTView[Int] = new IntSMTView[Int] {
    def smtView(a: Int): String = a.toString()
  }

  implicit val doubleSMT: RealSMTView[Double] = new RealSMTView[Double] {
    def smtView(a: Double): String = a.toString()
  }

  implicit def intToReal[A](implicit intView: IntSMTView[A]): RealSMTView[A] =
    new RealSMTView[A] {
      def smtView(a: A): String = s"(to_real ${intView.smtView(a)})"
    }
}
