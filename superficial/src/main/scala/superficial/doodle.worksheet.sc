import superficial._
import doodle.java2d._
import doodle.core._
import doodle.syntax._
val surf = PantsSurface.allClosed(2)(1)
val skewsurf = SkewPantsSurface.enumerate(surf, Vector(0.2))(0)
val skim = SkewPantsSurfaceImage(skewsurf, 100)
val pathslen2 = NormalPath.enumerate[SkewPantsHexagon](skewsurf, Some(2)).filter(p => (p.length == 2))
val clpathslen2 = pathslen2.filter(p => p.isClosed).toVector
val clp = clpathslen2(4)
val plps = PLPath.enumMinimal(clp, 0.05, 5).toVector
val arcMap = skim.facesWithPLPaths(Seq(plps(0) -> Color.red, plps(3000) -> Color.blue))
val arcImg = skim.imageGrid(arcMap)
arcImg.draw()
