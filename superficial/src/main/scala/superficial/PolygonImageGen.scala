package superficial

import doodle.core._
// import doodle.image.Image
import doodle.syntax._
import doodle.image.syntax._
import PathElement._
import cats.syntax._
import PolygonImageGen._
import Polygon.Index
import doodle.java2d._
import cats.implicits._

class PolygonImageGen(sides: Int, radius: Double = 100) {
  val centerAngle: Angle = 360.degrees / sides.toDouble

  val vertices: Vector[Point] = (0 until sides)
    .map(index => Point.polar(radius, centerAngle * index.toDouble))
    .toVector

  val edgePaths: Vector[Picture[Unit]] =
    vertices.zip(vertices.tail :+ vertices.head).toVector.map {
      case (init, term) =>
        OpenPath(List(moveTo(init), lineTo(term))).path
    }

  val edgePathsMid: Vector[(Picture[Unit], Point)] =
    vertices.zip(vertices.tail :+ vertices.head).toVector.map {
      case (init, term) =>
        (OpenPath(List(moveTo(init), lineTo(term))).path, convex(init, term, 0.5))
    }

  def onEdge(initial: Index, alpha: Double): Point =
    convex(vertices(initial), vertices((initial + 1) % sides), alpha)

  def normalArc(
      initialIndex: Int,
      initialDisplacement: Double,
      terminalIndex: Int,
      finalDisplacement: Double,
      width: Double = 2
  ): Picture[Unit] =
    OpenPath(
      List(
        moveTo(onEdge(initialIndex, initialDisplacement)),
        lineTo(onEdge(terminalIndex, finalDisplacement))
      )
    ).path.strokeWidth(width)

  def plArc(arc: PLArc, width: Double = 2): Picture[Unit] = {
    val (initDisp, termDisp) = relativeDisplacements(arc)
    normalArc(
      arc.base.initial,
      initDisp.doubleValue,
      arc.base.terminal,
      termDisp.doubleValue,
      width
    )

  }

  def plArcs(arcs: Seq[(PLArc, Color)]): doodle.algebra.Picture[Algebra,Drawing,Unit] =
    arcs.map {
      case (arc, colour) => plArc(arc).strokeColor(colour)
    }.reduce(_ on _)

  def thickPlArcs(arcs: Seq[(PLArc, Color, Int)]): doodle.algebra.Picture[Algebra,Drawing,Unit] =
    arcs.map {
      case (arc, colour, width) => plArc(arc, width).strokeColor(colour)
    }.reduce(_ on _)

}

object PolygonImageGen {
  def convex(initial: Point, terminal: Point, alpha: Double) =
    ((initial.toVec * (1.0 - alpha)) + (terminal.toVec * alpha)).toPoint

  def relativeDisplacements(arc: PLArc) = {
    (
      arc.initialDisplacement / arc.base.face.sideLength(arc.base.initialEdge),
      arc.finalDisplacement / arc.base.face.sideLength(arc.base.terminalEdge)
    )
  }
}
