package superficial

import Polygon.Index
import scala.collection.immutable.Nil
import superficial.Generator.a

object Triangles {
  case object X extends Vertex
  case object Y extends Vertex
  case object Z extends Vertex

  case object A extends EdgePair(X, Y)
  case object B extends EdgePair(Y, Z)
  case object C extends EdgePair(Z, X)

  val upper = Polygon(Vector(A.Positive, B.Positive, C.Positive))
  val lower = Polygon(Vector(C.Negative, B.Negative, A.Negative))

  object HollowTriangle extends TwoComplex {
    val sides = 3
    val faces = Set.empty
    val boundary = Vector(A.Positive, B.Positive, C.Positive)
    val vertices = Set(X, Y, Z)
    val edges = Set(A.Positive, B.Positive, C.Positive)
  }

  val doubleTriangle: TwoComplex = TwoComplex.pure(upper, lower)
}

import Triangles._