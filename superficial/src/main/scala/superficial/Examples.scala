package superficial

import Polygon.Index
import scala.collection.immutable.Nil
import superficial.Generator.a

object Triangles {
  import SphereComplex._

  object HollowTriangle extends TwoComplex {
    val sides = 3
    val faces = Set.empty
    val boundary = Vector(A.Positive, B.Positive, C.Positive)
    val vertices = Set(X, Y, Z)
    val edges = Set(A.Positive, B.Positive, C.Positive)
  }
}

object Examples {
  val disjointLoops =
    TwoComplex.symbolic("x", "y")("a" -> ("x", "x"), "b" -> ("y", "y"))()

  val wedgeTori = TwoComplex.symbolic("x")(
    "a1" -> ("x", "x"),
    "b1" -> ("x", "x"),
    "a2" -> ("x", "x"),
    "b2" -> ("x", "x")
  )(
    "face1" -> Seq("a1" -> true, "b1" -> true, "a1" -> false, "b1" -> false),
    "face2" -> Seq("a2" -> true, "b2" -> true, "a2" -> false, "b2" -> false)
  )
}
