package superficial
import scala.xml._
import math._
import scala.collection.immutable

object SvgPlot {
  def line(x1: Double,
           y1: Double,
           x2: Double,
           y2: Double,
           colour: String): Elem =
    <line x1={x1.toString} x2={x2.toString} y1={y1.toString} y2={y2.toString} stroke={colour} stroke-width="1" xmlns="http://www.w3.org/2000/svg"></line>

  def hexagonSides(offset: (Double, Double) = (0, 0),
              radius: Double = 100,
              colour: String = "black")
    : immutable.Seq[Elem] = {
    (0 to 5).map { j =>
      val (x, y) = offset
      val x1 = x + (radius / 2) + (cos(j * Pi / 3) * radius)
      val x2 = x + (radius / 2) + (cos((j + 1) * Pi / 3) * radius)
      val y1 = y + (radius / 2) + (sin(j * Pi / 3) * radius)
      val y2 = y + (radius / 2) + (sin((j + 1) * Pi / 3) * radius)
      line(x1, y1, x2, y2, colour)
    }
  }

  def svgPlot(elems: Seq[Elem]) =
      <svg width="100" height="100">elems </svg>
}
