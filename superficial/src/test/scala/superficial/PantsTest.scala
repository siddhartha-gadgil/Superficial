package superficial

import utest._

object PantsTest extends TestSuite{
    val tests: Tests = Tests{
        "ClosedCheck" - {
            "pants-surfaces" - {
                assert(PantsSurface.all(5).forall(_.isSurfaceWithBoundary))
                assert(PantsSurface.allClosed(4).forall(_.isClosedSurface))
            }
            "skew-surfaces" - {
                val twistSurfs = 
                PantsSurface.allClosed(4).flatMap(
                    s => SkewPantsSurface.enumerate(s, Vector(0, 0.2, 0.6)))
                assert(twistSurfs.forall(_.isClosedSurface))
            }
        }
    }
}