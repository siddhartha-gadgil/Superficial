package smtsolve
import freegroups._, UnitQuaternion._

final case class SU2Representations(
    numGenerators: Int,
    relations: Vector[Word],
    prefix: String = ""
) {
  val generators: Vector[Word] =
    (0 until (numGenerators)).map(j => Word(Vector(j + 1))).toVector

  val generatorReps: Vector[UnitQuaternion] =
    generators.map(fromWord(_, prefix))

  val nonTrivialEquation: BoolExpr = !generatorReps.map(_ =:= one).reduce(_ & _)

  val variables: Vector[RealExpr] = generatorReps.flatMap(_.components)

  val unitNormEquations: Vector[BoolExpr] = generatorReps.map(_.normEquation)

  val relationReps: Vector[UnitQuaternion] = relations.map(fromWord(_))

  val relationEquations: Vector[BoolExpr] = relationReps.map(_ =:= one)

  val allEquations
      : Vector[BoolExpr] = (relationEquations ++ unitNormEquations) :+ nonTrivialEquation

  val smtDoc = SMTDoc(variables, allEquations)
}

object SU2Representations {
  def apply(n: Int, words: String*): SU2Representations =
    SU2Representations(n, words.toVector.map(Word.fromString(_)))
}
