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
