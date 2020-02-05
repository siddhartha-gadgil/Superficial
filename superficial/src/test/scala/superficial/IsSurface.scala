package superficial

import utest._

import SphereComplex._, Examples._

object IsSurface extends TestSuite {
  val tests: Tests = Tests {
    "ClosedSurfaceCheck" - {
      "variouGenera" - {
          (0 to 10).foreach{
            j => 
              val surface = new StandardSurface(1 + j)
              assert(surface.isClosedSurface)
          }            
        }
      "disjointLoops" - {
        assert(!disjointLoops.isClosedSurface)
      }
      "polygon" - {
        val rnd = new util.Random
        val poly = Polygon(1 + rnd.nextInt(10))
        assert(!poly.isClosedSurface)
      }

      "wedgeOfTori" - {
          assert(!wedgeTori.isClosedSurface)
      }
    }

  }
}
