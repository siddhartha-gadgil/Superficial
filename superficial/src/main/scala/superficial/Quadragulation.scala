package superficial

import Polygon.Index
import scala.collection.immutable.Nil
import superficial.Generator.a

/**
 *a quadrilateral is a polygon with four edges.
 */
trait Quadrilateral extends Polygon {
    val side: Int = 4
}

