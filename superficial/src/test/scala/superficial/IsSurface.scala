package superficial

import utest._

import SphereComplex._, Examples._

object IsSurface extends TestSuite {
  val tests: Tests = Tests {
    "ClosedSurfaceCheck" - {
      "genusRandom" - {
        val rnd = new util.Random
        val surface = new StandardSurface(1 + rnd.nextInt(10))
        assert(surface.isClosedSurface)
      }
      "disjointLoops" - {
        assert(!disjointLoops.isClosedSurface)
      }
      "polygon" - {
        val rnd = new util.Random
        val poly = Polygon(1 + rnd.nextInt(10))
        assert(!poly.isClosedSurface)
      }
    }

  }
}
