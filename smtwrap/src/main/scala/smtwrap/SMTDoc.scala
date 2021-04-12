package smtwrap
import scala.util._
case class SMTDoc(
    variables: Vector[SMTExpr],
    claims: Vector[BoolSMTExpr] = Vector(),
    logic: String = "AUFNIRA",
    init: Vector[SMTCommand] = Vector(),
    actions: Vector[SMTCommand] = Vector()
) {
  require(!variables.exists(_.view.contains(" ")))

  val commandSeq: Vector[SMTCommand] =
    (init :+ SMTCommand(s"(set-logic $logic)")) ++
    variables.map(_.declare) ++
      claims.map(_.assert) ++
      actions

  val docText: String = commandSeq.map(_.text).mkString("", "\n", "\n")

  def addClaim(bs: BoolSMTExpr*): SMTDoc =
    this.copy(claims = claims ++ bs.toVector)

  def addVars(exps: SMTExpr*): SMTDoc =
    this.copy(variables = variables ++ exps.toVector)

  def addCheck: SMTDoc =
    this.copy(actions = actions :+ SMTCommand("(check-sat)"))

  def valueGetter = {
    val newInit = init :+ SMTCommand.produceModels
    val getValCommand = SMTCommand.getValues(variables)
    val newActions =
      if (actions.lastOption.exists(_.text.replace(" ", "") == "(check-sat)"))
        actions :+ getValCommand
      else actions ++ Vector(SMTCommand("(check-sat)"), getValCommand)
    this.copy(init = newInit, actions = newActions)
  }

  val filename = s"smtdoc-$hashCode.smt2"

  def writeDoc(extraInits: Vector[SMTCommand] = Vector(), extraActions: Vector[SMTCommand] = Vector()): Unit = {
      os.write.over(os.pwd / filename, SMTDoc.doc(extraInits ++ commandSeq ++ extraActions))
  }

  def z3Run() = {
      writeDoc()
      os.proc("z3", "-smt2", filename).call()
  }

  val z3Interactive = Vector("z3", "-in")

  def seekValues(commands: Vector[String] = z3Interactive): Either[String,Map[String,String]] = {
      val proc = os.proc((commands).map(os.Shellable.StringShellable(_)) : _*).spawn()
      commandSeq.foreach(cmd => proc.stdin.writeLine(cmd.text))
      proc.stdin.writeLine("(check-sat)")
      proc.stdin.flush()
      val result = proc.stdout.readLine()
      if (result == "sat") {
          proc.stdin.writeLine(SMTCommand.getValues(variables).text)
          proc.stdin.writeLine("(exit)")
          proc.stdin.flush()
          val valueString = new String(proc.stdout.readAllBytes()) 
          Right(SMTDoc.recValues(valueString.trim().drop(1)))
      } else Left(result)
  }
}

object SMTDoc{
    def recValues(data: String, accum: Map[String, String] = Map()): Map[String, String] = data match {
        case s"($headKey $headValue)$tail" => recValues(tail.trim(), accum + (headKey.trim -> headValue.trim))
        case _ =>  accum
    }

    def doc(commands: Vector[SMTCommand]) = commands.map(_.text).mkString("", "\n", "\n")

    def parseValues(results: List[String]): Either[String,Map[String,String]] = results match{
        case List("sat", s"(${data})") =>             
            Right(recValues(data.trim, Map()))
        case List("unsat") => Left("unsat")
        case List("unknown") => Left("unknown")
        case _ => throw new Exception(s"Could not parse $results as a map of values")
    }
}