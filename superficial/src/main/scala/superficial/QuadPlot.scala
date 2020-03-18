package superficial

import SvgPlot._
import scala.xml._
import math._

class QuadPlot(val genus: Int, radius: Double = 200) {
  def vertexPosition(k: Int): (Double, Double) =
    (
      1.5 * radius + (radius * cos(2 * Pi * k.toDouble / (4 * genus))),
      1.5 * radius + (radius * sin(2 * Pi * k.toDouble / (4 * genus)))
    )

  def aEdge(k: Int) = {
    val (x1, y1) = vertexPosition(4 * k)
    val (x2, y2) = vertexPosition(4 * k + 1)
    lineArrow(x1, y1, x2, y2, getColour(2 * k + 1), s"a${k + 1}")
  }

  def bEdge(k: Int) = {
    val (x1, y1) = vertexPosition(4 * k + 1)
    val (x2, y2) = vertexPosition(4 * k + 2)
    lineArrow(x1, y1, x2, y2, getColour(2 * k), s"b${k + 1}")
  }

  def aBarEdge(k: Int) = {
    val (x1, y1) = vertexPosition(4 * k + 3)
    val (x2, y2) = vertexPosition(4 * k + 2)
    lineArrow(x1, y1, x2, y2, getColour(2 * k + 1), s"a${k + 1}!")
  }

  def bBarEdge(k: Int) = {
    val (x1, y1) = vertexPosition(4 * k + 4)
    val (x2, y2) = vertexPosition(4 * k + 3)
    lineArrow(x1, y1, x2, y2, getColour(2 * k), s"b${k + 1}!")
  }

  lazy val sides: List[Elem] =
    (0 until (genus)).toList.flatMap(
      k => aEdge(k) ++ bEdge(k) ++ aBarEdge(k) ++ bBarEdge(k)
    )

  def perturbedVertex(k: Int, shift: Double) = {
      println(k)
      println(k % 4)
      println((k + (4 * genus) - 1) % (4 * genus))
      println()
    val (x1, y1) = vertexPosition(k)
    val (x2, y2) =
      if (k % 4 < 2) vertexPosition(k + 1)
      else vertexPosition((k + (4 * genus) - 1) % (4 * genus))
    (x1 * (1 - shift) + (x2 * shift), y1 * (1 - shift) + (y2 * shift))
  }

  def perturbedCentre(index: Int, shift: Double) =
    ((1.5 + (shift * 0.7 * index)) * radius, (1.5 + (shift * index)) * radius)

  val V = ClosedSurface.V

  val F = ClosedSurface.Face(genus)

  import Quadrangulation._

  def quadEdges(path: EdgePath): Vector[Quadrangulation.QuadEdge] =
    EdgePath.edgeVectors(path).collect { case qp @ QuadEdge(F, n, _) => qp }

  def edgeLine(qe: QuadEdge, index: Int, length: Int, shift: Double) : Vector[Elem] = {
    val (bIndex, vIndex) =
      if (qe.positivelyOriented) (index, (index + 1) % length) else ((index + 1) % length, index)
    val (bx, by) = perturbedCentre(bIndex, shift)
    val (vx, vy) = vertexPosition(qe.index)
    val colour = shiftColour(vIndex)
    if (qe.positivelyOriented)
      lineArrow(bx, by, vx, vy, colour, index.toString())
    else lineArrow(vx, vy, bx, by, colour, index.toString())
  }

  def vecEdgeLines(qes: Vector[QuadEdge], index: Int, length: Int, shift: Double) : Vector[Elem] = 
    qes match {
        case Vector() => Vector()
        case head +: tail => edgeLine(head, index, length, shift) ++ vecEdgeLines(tail, (index + 1) % length, length, shift)
    }

  def quadEdgeLines(path: EdgePath, shift: Double) =
    vecEdgeLines(quadEdges(path), 0, EdgePath.length(path), shift)

  def quadEdgeSVG(path: EdgePath, shift: Double = 0.1) = 
    svgPlot(sides.toVector ++ quadEdgeLines(path, shift), 3 * radius, 3 * radius)  
}
