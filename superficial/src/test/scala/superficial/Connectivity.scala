package superficial

import utest._

import SphereComplex._, Examples._

object Connectivity extends TestSuite {
  val tests: Tests = Tests {
    "AllSphereComplexes" - {
      "areConnected" - {
        assert(
          doubleTriangle.isConnectedComplex && doubleBigon.isConnectedComplex && doubleMonogon.isConnectedComplex
        )
      }
    }
    "Connectivity" - {
        "disjointLoops" -{
            assert(!Examples.disjointLoops.isConnectedComplex)
        } 
        "genusRandom" - {
            val rnd = new util.Random
            val surface = new StandardSurface(1 + rnd.nextInt(10))
            assert(surface.isConnectedComplex)
        }
    }
    "wedgeOfTori" - {
          assert(wedgeTori.isConnectedComplex)
      }
  }
}
