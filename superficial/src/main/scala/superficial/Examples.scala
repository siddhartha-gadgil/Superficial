package superficial

import EdgePair._
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

  val pinchedTorus = 
    TwoComplex.symbolic("V1")(
      "E1" -> ("V1", "V1"), "E2" -> ("V1", "V1")
      )(
        "F1" -> Seq("E1" -> true, "E2" -> false), "F2" -> Seq("E2" -> true, "E1" -> false)
      )

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
  val disk =  TwoComplex.symbolic("V")(
    "a1" -> ("V", "V")
  )(
    "face1" -> Seq("a1" -> true)
  )
            
  val torusWithBoundary = TwoComplex.symbolic("v")(
    "a1" -> ("v", "v"), "a2" -> ("v", "v"), "ab" -> ("v", "v") 
    )(
      "F" -> Seq("a1" -> true, "ab" -> true, "a2" -> true, "a1" -> false, "a2" -> false)
  )
  
}
