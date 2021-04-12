package smtwrap

// quick and dirty implementation with weak typing, for instance not recognizing boolean expressions.
sealed trait SMTExpr {
  val view: String

  val sort: String

  lazy val declare = {
    require(!view.contains(" "), s"$view is not a variable")
    SMTCommand(s"(declare-fun $view() $sort)")
  }
}

case class IntExpr(view: String) extends SMTExpr with IntOps {
  def toReal = RealExpr(s"(to_real $view)")

  val sort: String = "Int"
}

case class BoolExpr(view: String) extends SMTExpr with BoolOps {
  val sort: String = "Bool"
}

object BoolExpr {
  def apply(bool: Boolean): BoolExpr = BoolExpr(bool.toString())
}

case class RealExpr(view: String) extends SMTExpr with RealOps {
  val realView: String = view

  val sort: String = "Real"
}

object RealExpr{
    def apply(x: Double): RealExpr = RealExpr(x.toString())
}

case class SMTCommand(text: String)

object SMTCommand {
  val produceModels = SMTCommand("(set-option :produce-models true)")

  val checkSat = SMTCommand("(check-sat)")

  def getValues(variables: Vector[SMTExpr]) =
    SMTCommand(s"(get-value (${variables.map(_.view).mkString(" ")}))")
}
trait SMTOps {
  val view: String

  def op[A: SMTView](opName: String, that: A) =
    s"($opName $view ${implicitly[SMTView[A]].smtView(that)})"

}

trait IntOps extends SMTOps {
  lazy val intView = view

  def intOp[A: IntSMTView](opName: String, that: A) =
    IntExpr(s"($opName $intView ${implicitly[IntSMTView[A]].smtView(that)})")

  def intBoolOp[A: IntSMTView](opName: String, that: A) =
    BoolExpr(
      s"($opName $intView ${implicitly[IntSMTView[A]].smtView(that)})"
    )

  def =:=[A: IntSMTView](that: A) = intOp("=", that)

  def +[A: IntSMTView](that: A) = intOp("+", that)

  def -[A: IntSMTView](that: A) = intOp("-", that)

  def *[A: IntSMTView](that: A) = intOp("*", that)

  def <[A: IntSMTView](that: A) = intBoolOp("<", that)

  def >[A: IntSMTView](that: A) = intBoolOp(">", that)

  def <=[A: IntSMTView](that: A) = intBoolOp("<=", that)

  def >=[A: IntSMTView](that: A) = intBoolOp(">=", that)

  def unary_- = IntExpr(s"(- $view)")
}

trait RealOps extends SMTOps {
  val realView: String

  def realOp[A: RealSMTView](opName: String, that: A) =
    RealExpr(
      s"($opName $realView ${implicitly[RealSMTView[A]].smtView(that)})"
    )
  def realBoolOp[A: RealSMTView](opName: String, that: A) =
    BoolExpr(
      s"($opName $realView ${implicitly[RealSMTView[A]].smtView(that)})"
    )

  def =:=[A: RealSMTView](that: A) = realBoolOp("=", that)

  def +[A: RealSMTView](that: A) = realOp("+", that)

  def -[A: RealSMTView](that: A) = realOp("-", that)

  def *[A: RealSMTView](that: A) = realOp("*", that)

  def /[A: RealSMTView](that: A) = realOp("/", that)

  def <[A: RealSMTView](that: A) = realBoolOp("<", that)

  def >[A: RealSMTView](that: A) = realBoolOp(">", that)

  def <=[A: RealSMTView](that: A) = realBoolOp("<=", that)

  def >=[A: RealSMTView](that: A) = realBoolOp(">=", that)

  def unary_- = RealExpr(s"(- $view)")
}

trait BoolOps extends SMTOps {
  def assert = SMTCommand(s"(assert $view)")

  def &&(that: BoolExpr) = BoolExpr(s"(and $view ${that.view})")

  def ||(that: BoolExpr) = BoolExpr(s"(or $view ${that.view})")

  def unary_! = BoolExpr(s"(not $view)")
}

object SMTExpr {
  def double(name: Double) = IntExpr(name.toString()) // FIXME: quick fix

  def int(name: Int) = IntExpr(name.toString())

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

  implicit val intSmtWithView: IntSMTView[IntExpr] =
    new IntSMTView[IntExpr] {
      def smtView(a: IntExpr): String = a.view
    }

  implicit val boolSmtWithView: BoolSMTView[BoolExpr] =
    new BoolSMTView[BoolExpr] {
      def smtView(a: BoolExpr): String = a.view
    }

  implicit val realSmtWithView: RealSMTView[RealExpr] =
    new RealSMTView[RealExpr] {
      def smtView(a: RealExpr): String = a.view
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
