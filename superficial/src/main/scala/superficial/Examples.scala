package superficial

import Polygon.Index
import scala.collection.immutable.Nil
import superficial.Generator.a

case object X extends Vertex
case object Y extends Vertex
case object Z extends Vertex

case object A extends EdgePair(X, Y)
case object B extends EdgePair(Y, Z)
case object C extends EdgePair(Z, X)

object Triangle extends TwoComplex {
  val sides = 3
  val faces = Set.empty
  val boundary = Vector(A.Positive, B.Positive, C.Positive)
  val vertices = Set(X, Y, Z)
  val edges = Set(A.Positive, B.Positive, C.Positive)
}