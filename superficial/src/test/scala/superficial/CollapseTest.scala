package superficial

import utest._

import SphereComplex._

object CollapseTest extends TestSuite {
  val tests: Tests = Tests {
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
  }
}
