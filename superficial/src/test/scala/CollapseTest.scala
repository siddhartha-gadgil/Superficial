package superficial

import utest._

object SphereComplex {
  case object X extends Vertex
  case object Y extends Vertex
  case object Z extends Vertex

  case object A extends EdgePair(X, Y)
  case object B extends EdgePair(Y, Z)
  case object C extends EdgePair(Z, X)

  val upper = Polygon(Vector(A.Positive, B.Positive, C.Positive))
  val lower = Polygon(Vector(C.Negative, B.Negative, A.Negative))

  val doubleTriangle: TwoComplex = TwoComplex.pure(upper, lower)

  val doubleBigon : TwoComplex = doubleTriangle.collapseEdge(A.Positive)
  val doubleMonogon = TwoComplex.allCollapsed(doubleTriangle)
}
import SphereComplex._

object CollapseTest extends TestSuite {
    val tests: Tests = Tests{
        "collapseA" - {
            "twoedges" - {
                assert(doubleBigon.edges.size == 4)
            }
            "vertexSet" - {
                assert(doubleBigon.vertices == Set(X, Z))
            }
        "fullCollapse" - {
            "oneVertex" - {
                assert(doubleMonogon.vertices.size == 1)
            }
            "twoEdges" - {
                assert(doubleMonogon.edges.size == 2)
            }
            "edgesFlipsOfEachOther" - {
                val e = doubleMonogon.edges.head
                assert(doubleMonogon.edges == Set(e, e.flip))
            }
        }
    }
    "AllComplexes" - {
            "areConnected" - {
                assert(doubleTriangle.isConnectedComplex && doubleBigon.isConnectedComplex&& doubleMonogon.isConnectedComplex)
            }
        }
    }
}

