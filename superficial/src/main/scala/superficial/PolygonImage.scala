package superficial

import doodle.core._
import doodle.image.Image
import doodle.syntax._
import doodle.image.syntax._
import PathElement._
import cats.syntax._
import PolygonImage._
import Polygon.Index

class PolygonImage(sides: Int, radius: Double = 100) {
  val centerAngle: Angle = 360.degrees / sides.toDouble

  val vertices: Vector[Point] = (0 until sides)
    .map(index => Point.polar(radius, centerAngle * index.toDouble))
    .toVector

  val edgePaths: Vector[OpenPath] =
    vertices.zip(vertices.tail :+ vertices.head).toVector.map {
      case (init, term) => OpenPath(List(moveTo(init), lineTo(term)))
    }

  def onEdge(initial: Index, alpha: Double): Point =
    convex(vertices(initial), vertices((initial + 1) % sides), alpha)

  def normalArc(
      initialIndex: Int,
      initialDisplacement: Double,
      terminalIndex: Int,
      finalDisplacement: Double
  ): OpenPath =
    OpenPath(
      List(
        moveTo(onEdge(initialIndex, initialDisplacement)),
        lineTo(onEdge(terminalIndex, finalDisplacement))
      )
    )

  def plArc(arc: PLArc): OpenPath = {
    val (initDisp, termDisp) = relativeDisplacements(arc)
    normalArc(
      arc.base.initial,
      initDisp.doubleValue,
      arc.base.terminal,
      termDisp.doubleValue
    )
  }

}

object PolygonImage {
  def convex(initial: Point, terminal: Point, alpha: Double) =
    ((initial.toVec * (1.0 - alpha)) + (terminal.toVec * alpha)).toPoint

  def relativeDisplacements(arc: PLArc) = {
    (
      arc.initialDisplacement / arc.base.face.edgeLengths(arc.base.initial).get,
      arc.finalDisplacement / arc.base.face.edgeLengths(arc.base.terminal).get
    )
  }
}
