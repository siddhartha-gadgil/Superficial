package superficial

import utest._

import SphereComplex._

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
        "twoLoops" -{
            assert(!Examples.disConnected.isConnectedComplex)
        } 
        "genusRandom" - {
            val rnd = new util.Random
            val surface = new StandardSurface(rnd.nextInt(10))
            assert(surface.isConnectedComplex)
        }
    }
  }
}
