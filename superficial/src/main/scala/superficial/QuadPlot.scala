package superficial

import SvgPlot._
import scala.xml._
import math._

class QuadPlot(val genus: Int, radius: Double = 100) {
  def vertexPosition(k: Int): (Double, Double) =
    (
      1.5 * radius + (radius * cos(2 * Pi * k.toDouble /  (4 * genus))),
      1.5 * radius + (radius * sin(2 * Pi * k.toDouble /  (4 * genus)))
    )

  def aEdge(k: Int) = {
    val (x1, y1) = vertexPosition(4 * k)
    val (x2, y2) = vertexPosition(4 * k + 1)
    lineArrow(x1, y1, x2, y2, getColour(2 * k + 1), s"a$k")
  }

  def bEdge(k: Int) = {
    val (x1, y1) = vertexPosition(4 * k + 1)
    val (x2, y2) = vertexPosition(4 * k + 2)
    lineArrow(x1, y1, x2, y2, getColour(2 * k), s"b$k")
  }

  def aBarEdge(k: Int) = {
    val (x1, y1) = vertexPosition(4 * k + 3)
    val (x2, y2) = vertexPosition(4 * k + 2)
    lineArrow(x1, y1, x2, y2, getColour(2 * k + 1), s"a$k!")
  }

  def bBarEdge(k: Int) = {
    val (x1, y1) = vertexPosition(4 * k + 4)
    val (x2, y2) = vertexPosition(4 * k + 3)
    lineArrow(x1, y1, x2, y2, getColour(2 * k), s"b$k!")
  }

  lazy val sides: List[Elem] =
    (0 until (genus)).toList.flatMap(
      k => aEdge(k) ++ bEdge(k) ++ aBarEdge(k) ++ bBarEdge(k)
    )

  def perturbedVertex(k: Int, shift: Double) = {
      val (x1, y1) = vertexPosition(k)
      val (x2, y2) = vertexPosition(k + 1)
      (x1 * (1 - shift) + (x2 * shift), y1 * (1 - shift) + (y2 * shift))
  }

  def perturbedCentre(shift: Double) = (1.5 * radius, (1.5 + shift) * radius)

  val V = ClosedSurface.V

  val F = ClosedSurface.Face(genus)

  import Quadrangulation._

  
}
