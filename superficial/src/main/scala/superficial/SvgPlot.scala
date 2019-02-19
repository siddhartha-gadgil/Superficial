package superficial
import scala.xml._
import math._
import scala.collection.immutable

object SvgPlot {
  def drawLine(x1: Double,
               y1: Double,
               x2: Double,
               y2: Double,
               colour: String): Elem =
    <line x1={x1.toInt.toString} x2={x2.toInt.toString} y1={y1.toInt.toString} y2={y2.toInt.toString} stroke={colour} stroke-width="1" xmlns="http://www.w3.org/2000/svg"></line>

  val colours: Vector[String] = Vector("blue",
                       "green",
                       "black",
                       "orange",
                       "cyan",
                       "red",
                       "brown",
                       "grey",
                       "magenta")

  def hexagonSides(offset: (Double, Double) = (0, 0),
                   radius: Double = 100): immutable.Seq[Elem] = {
    (0 to 5).flatMap { j =>
      val (x, y) = offset
      val x1 = x + (radius) + (cos(j * Pi / 3) * radius)
      val x2 = x + (radius) + (cos((j + 1) * Pi / 3) * radius)
      val y1 = y + (radius) + (sin(j * Pi / 3) * radius)
      val y2 = y + (radius) + (sin((j + 1) * Pi / 3) * radius)
      lineArrow(x1, y1, x2, y2, colours(j % (colours.size)))
    }
  }

  def unit(x: Double, y: Double): (Double, Double) =
    (x / sqrt(x * x + y * y), y / sqrt(x * x + y * y))

  val rad: Int = 5

  def lineArrow(xinit: Double,
                yinit: Double,
                xt: Double,
                yterm: Double,
                colour: String = "black"): Vector[Elem] = {

    val arrowBase = ((xt * 3 + xinit) / 4, (yterm * 3 + yinit) / 4)
    val (bu, tu) = arrowBase
    val (xu, yu) = unit(xt - xinit, yterm - yinit)
    Vector(
      drawLine(xinit, yinit, xt, yterm, colour),
      drawLine(bu,
               tu,
               bu - (xu * rad) - (yu * rad),
               tu - (yu * rad) + (xu * rad),
               "black"),
      drawLine(bu,
               tu,
               bu - (xu * rad) + (yu * rad),
               tu - (yu * rad) - (xu * rad),
               "black")
    )
  }

  def svgPlot(elems: Seq[Elem]): Elem =
    <svg version="1.1"
           baseProfile="full"
           width="300" height="200"
           xmlns="http://www.w3.org/2000/svg">
           {elems} </svg>

  val eg: Elem = svgPlot(hexagonSides())

}
