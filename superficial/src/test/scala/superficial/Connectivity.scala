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
        "variouGenera" - {
          (0 to 10).foreach{
            j => 
              val surface = new StandardSurface(1 + j)
              assert(surface.isConnectedComplex)
          }            
        }
    }
    "wedgeOfTori" - {
          assert(wedgeTori.isConnectedComplex)
      }
  }
}
