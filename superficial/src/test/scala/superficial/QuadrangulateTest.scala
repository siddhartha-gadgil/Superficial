package superficial

import utest._

import SphereComplex._, Examples._

object QuadrangulateTest extends TestSuite {
  val tests: Tests = Tests {
    "variouGenera" - {
      (0 to 10).foreach { j =>
        val g = j + 1
        val base = new StandardSurface(1 + j)
        val surface = base.quadrangulate
        assert(surface.checkComplex)
        assert(surface.isClosedSurface)
        assert(surface.isConnectedComplex)
        assert(surface.vertices.size == 2)
        assert(surface.faces.size == 2 * g)
        assert(surface.edges.size == 8 * g)
        assert(surface.chi == base.chi)
      }
    }
    "doubleBigon" - {
      val surface = doubleBigon.quadrangulate
      assert(surface.checkComplex)
      assert(surface.isClosedSurface)
      assert(surface.isConnectedComplex)
      assert(surface.vertices.size == 4)
      assert(surface.chi == 2)
    }
    "doubleMonogon" - {
      val surface = doubleMonogon.quadrangulate
      assert(surface.checkComplex)
      assert(surface.isClosedSurface)
      assert(surface.isConnectedComplex)
      assert(surface.vertices.size == 3)
      assert(surface.chi == 2)
    }
    "doubleTriangle" - {
      val surface = doubleTriangle.quadrangulate
      assert(surface.checkComplex)
      assert(surface.isClosedSurface)
      assert(surface.isConnectedComplex)
      assert(surface.vertices.size == 5)
      assert(surface.chi == 2)
    }
  }
}
