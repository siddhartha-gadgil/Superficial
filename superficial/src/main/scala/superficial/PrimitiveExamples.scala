package superficial

import StandardSurface._
import DoublePaths._
import Quadrangulation._
import NonPosQuad._

object PrimitiveExamples {
    /**
    val genus2 = ClosedSurface(2)
    val (genus2quad1, (fwd2, back2)) = Quadrangulation.quadrangulate(genus2)
    val genus2quad = NonPosQuad(genus2quad1)
    val a1_b1_genus2 = DoublePaths.geometricIntersection(fwd2(genus2.getPath("a1")), fwd2(genus2.getPath("b1")), genus2quad)
    val b1_a1_genus2 = DoublePaths.geometricIntersection(fwd2(genus2.getPath("b1")), fwd2(genus2.getPath("a1")), genus2quad)
    val a1_b2_genus2 = DoublePaths.geometricIntersection(fwd2(genus2.getPath("a1")), fwd2(genus2.getPath("b2")), genus2quad)
    val a1_b1inv_genus2 = DoublePaths.geometricIntersection(fwd2(genus2.getPath("a1")), fwd2(genus2.getPath("b1!")), genus2quad)
    val a1_b2inv_genus2 = DoublePaths.geometricIntersection(fwd2(genus2.getPath("a1")), fwd2(genus2.getPath("b2!")), genus2quad)
    val a1a2_self_genus2 = DoublePaths.geometricIntersection(fwd2(genus2.getPath("a1a2")), fwd2(genus2.getPath("a1a2")), genus2quad)
    val a1a2inv_self_genus2 = DoublePaths.geometricIntersection(fwd2(genus2.getPath("a1a2!")), fwd2(genus2.getPath("a1a2!")), genus2quad)
    val com_a1a2_genus2 = DoublePaths.geometricIntersection(fwd2(genus2.getPath("a1b1a1!b1!")), fwd2(genus2.getPath("a1a2")), genus2quad)
    def generateHigherStrings(n: Int, start: String): String = {
        if(n <= 0) start
        else generateHigherStrings(n-1, start.concat("b1"))
    }
    */

}