import superficial._
import doodle.java2d._
import doodle.core._
import doodle.syntax._
import SkewPantsSurfaceImage._

// choosing a surface
val surf = PantsSurface.allClosed(2)(1)
val skewsurf = SkewPantsSurface.enumerate(surf, Vector(0.2))(0)

// choosing PL curves to plot
val pathslen2 = NormalPath.enumerate[SkewPantsHexagon](skewsurf, Some(2)).filter(p => (p.length == 2))
val clpathslen2 = pathslen2.filter(p => p.isClosed).toVector
val clp = clpathslen2(4)
val plps = PLPath.enumMinimal(clp, 0.05, 5).toVector

// the plotting given a surface and PL curves
val arcImg = arcPairImage(skewsurf, plps(0), plps(3000))
arcImg.draw()
