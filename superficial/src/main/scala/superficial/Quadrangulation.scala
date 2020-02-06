package superficial

import Polygon.Index
import scala.collection.immutable.Nil
import superficial.Generator.a

/**
 *a quadrilateral is a polygon with four edges.
 */
trait Quadrilateral extends Polygon {
    val sides: Int = 4
}

trait Quadrangulation extends TwoComplex{
    
}

object Quadrangulation{
    def apply(twoComplex: TwoComplex): Quadrangulation = {
        assert(twoComplex.isClosedSurface)
        assert(twoComplex.faces.forall(_.sides == 4))
        twoComplex.asInstanceOf[Quadrangulation]
    }
}