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


  def getColour(n: Int) = 
    s"hsl(${n * 81}, 100%, 50%)"
    // colours(n % (colours.size))

  def hexagonSides(offset: (Double, Double) = (0, 0),
                   radius: Double = 100): immutable.Seq[Elem] = {
    (0 to 5).flatMap { j =>
      val (x, y) = offset
      val x1 = x + (radius) + (cos(j * Pi / 3) * radius)
      val x2 = x + (radius) + (cos((j + 1) * Pi / 3) * radius)
      val y1 = y + (radius) + (sin(j * Pi / 3) * radius)
      val y2 = y + (radius) + (sin((j + 1) * Pi / 3) * radius)
      lineArrow(x1, y1, x2, y2, getColour(j))
    }
  }

  def pantsHexagonSides(
      hex: PantsHexagon,
      complex: TwoComplex,
      offset: (Double, Double) = (0, 0),
                   radius: Double = 100): immutable.Seq[Elem] = {
    hex.boundary.zipWithIndex.flatMap { case (e, j) =>
      val (ind, pos) = complex.edgeIndex(e).get
      val (x, y) = offset
      val x1 = x + (radius) + (cos(j * Pi / 3) * radius)
      val x2 = x + (radius) + (cos((j + 1) * Pi / 3) * radius)
      val y1 = y + (radius) + (sin(j * Pi / 3) * radius)
      val y2 = y + (radius) + (sin((j + 1) * Pi / 3) * radius)
      if (pos) lineArrow(x1, y1, x2, y2, getColour(ind), (ind + 1).toString)
      else lineArrow(x2, y2, x1, y1, getColour(ind), (ind + 1).toString)
    }
  }

  def allHexagonSides(complex: PantsSurface) = {
    complex.faces.toVector.collect{case ph : PantsHexagon => ph}.flatMap{
      hex =>
        val offset = (150.0 * (hex.pants) + 25.0, if (hex.top) 25.0 else 175.0)
        pantsHexagonSides(hex, complex, offset, 50)
    }
  }

  def unit(x: Double, y: Double): (Double, Double) =
    (x / sqrt(x * x + y * y), y / sqrt(x * x + y * y))

  val rad: Int = 3

  def lineArrow(xinit: Double,
                yinit: Double,
                xt: Double,
                yterm: Double,
                colour: String = "black",
                label: String = ""): Vector[Elem] = {

    val arrowBase = ((xt * 3 + xinit) / 4, (yterm * 3 + yinit) / 4)
    val (bu, tu) = arrowBase
    val (bt, tt) = ((xt + (3 * xinit)) / 4, (yterm + (3 * yinit)) / 4)
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
               "black"),
               <text x={bt.toInt.toString} y={tt.toInt.toString}>{label}</text>
    )
  }

  def svgPlot(elems: Seq[Elem], width: Int = 1000, height: Int = 400): Elem =
    <svg version="1.1"
           baseProfile="full"
           viewBox={s"0 0 $width $height"}
           xmlns="http://www.w3.org/2000/svg">
           {elems} </svg>

  val eg: Elem = svgPlot(hexagonSides())

  def plotSurface(complex: PantsSurface) = svgPlot(allHexagonSides(complex), 150 * math.max(complex.numPants, 4), 300)

}
