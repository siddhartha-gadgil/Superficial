package smtsolve

sealed trait SMTResult {}

case object Unknown extends SMTResult

case class Sat(m: Map[SMTExpr, String]) extends SMTResult

case class Unsat(proof: String) extends SMTResult

object SMTResult {
  val initCommands =
    Vector(SMTCommand.produceModels, SMTCommand.produceProofs)
  import SMTDoc._

  def seek(doc: SMTDoc, commands: Vector[String] = z3Interactive) : SMTResult = {
    import doc._
    val proc =
      os.proc((commands).map(os.Shellable.StringShellable(_)): _*).spawn()
    (initCommands ++ doc.commandSeq)
      .foreach(cmd => proc.stdin.writeLine(cmd.text))
    proc.stdin.writeLine("(check-sat)")
    proc.stdin.flush()
    val result = proc.stdout.readLine().trim()
    result match {
      case "sat" =>
        proc.stdin.writeLine(SMTCommand.getValues(variables).text)
        proc.stdin.writeLine("(exit)")
        proc.stdin.flush()
        val valueString = new String(proc.stdout.readAllBytes())
        // println(valueString)
        // println(SExpression.get(valueString))
        // println(SExpression.getMap(valueString))
        val m = 
            SExpression.getMap(valueString)
        // recValues(valueString.trim().drop(1).dropRight(1))
        val mVar = m.flatMap {
          case (name, value) =>
            variables.find(_.view == name).map(v => v -> value)
        }
        Sat(mVar)
      case "unknown" => Unknown
      case "unsat" => 
        proc.stdin.writeLine("(get-proof)")
        proc.stdin.writeLine("(exit)")
        proc.stdin.flush()
        val proofString = new String(proc.stdout.readAllBytes())
        Unsat(proofString)
      case err => throw new Exception(s"Could not recognize response $err; expecting sat/unsat/unknown")
    }
  }
}
