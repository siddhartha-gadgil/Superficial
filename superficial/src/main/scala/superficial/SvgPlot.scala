package superficial
import scala.xml._
import math._
import scala.collection.immutable
import scala.collection.convert.impl.IntVectorStepper
import java.io._
import java.awt.Desktop
import java.net.URI

object SvgPlot {
  def drawLine(
      x1: Double,
      y1: Double,
      x2: Double,
      y2: Double,
      colour: String
  ): Elem =
    <line x1={x1.toInt.toString} x2={x2.toInt.toString} y1={y1.toInt.toString} y2={
      y2.toInt.toString
    } stroke={colour} stroke-width="1" xmlns="http://www.w3.org/2000/svg"></line>

  def drawCircle(
    cx: Double,
    cy: Double,
    colour: String,
    r: Double = 3
  ) = 
    <circle cx={cx.toInt.toString} cy={cy.toInt.toString} r={r.toInt.toString} fill={colour}/>

  def getColour(n: Int) =
    s"hsl(${(n * 81) % 360}, 100%, 50%)"
  
  def shiftColour(n: Int) =
    s"hsl(${(40 +  (n * 81)) % 360}, 100%, 50%)"

  def hexagonSides(
      offset: (Double, Double) = (0, 0),
      radius: Double = 100
  ): immutable.Seq[Elem] = {
    (0 to 5).flatMap { j =>
      val (x, y) = offset
      val x1 = x + (radius) + (cos(j * Pi / 3) * radius)
      val x2 = x + (radius) + (cos((j + 1) * Pi / 3) * radius)
      val y1 = y + (radius) + (sin(j * Pi / 3) * radius)
      val y2 = y + (radius) + (sin((j + 1) * Pi / 3) * radius)
      lineArrow(x1, y1, x2, y2, getColour(j))
    }
  }

  def faceSides(
      face: Polygon,
      complex: TwoComplex,
      offset: (Double, Double) = (0, 0),
      radius: Double = 100
  ): Vector[Elem] = {
    val theta = 2* Pi / face.sides
    face.boundary.zipWithIndex.flatMap {
      case (e, j) =>
        val (ind, pos) = complex.edgeIndex(e).get
        val (x, y) = offset
        val x1 = x + (radius) + (cos(j * theta) * radius)
        val x2 = x + (radius) + (cos((j + 1) * theta) * radius)
        val y1 = y + (radius) + (sin(j * theta) * radius)
        val y2 = y + (radius) + (sin((j + 1) * theta) * radius)
        val circle = drawCircle(x1, y1, shiftColour(complex.vertexIndex(e.initial).get))
        if (pos) lineArrow(x1, y1, x2, y2, getColour(ind), (ind + 1).toString) :+ circle
        else lineArrow(x2, y2, x1, y1, getColour(ind), (ind + 1).toString) :+ circle
    }
  }

  def allHexagonSides(complex: PantsSurface) : Vector[Elem] = {
    complex.faces.toVector.collect { case ph: PantsHexagon => ph }.flatMap {
      hex =>
        val offset = (150.0 * (hex.pants) + 25.0, if (hex.top) 25.0 else 175.0)
        faceSides(hex, complex, offset, 50)
    }
  }

  def faceRowSides(complex: TwoComplex, r: Double = 50) = {
    def offset(n: Int) = (r/2 + (3 * r * n), r/2)
    complex.faces.toVector.zipWithIndex.flatMap{
      case (face, n) => faceSides(face, complex, offset(n), r)
    }
  }

  def unit(x: Double, y: Double): (Double, Double) =
    (x / sqrt(x * x + y * y), y / sqrt(x * x + y * y))

  val rad: Int = 3

  def lineArrow(
      xinit: Double,
      yinit: Double,
      xt: Double,
      yterm: Double,
      colour: String = "black",
      label: String = ""
  ): Vector[Elem] = {

    val arrowBase = ((xt * 3 + xinit) / 4, (yterm * 3 + yinit) / 4)
    val (bu, tu) = arrowBase
    val (bt, tt) = ((xt + (3 * xinit)) / 4, (yterm + (3 * yinit)) / 4)
    val (xu, yu) = unit(xt - xinit, yterm - yinit)
    Vector(
      drawLine(xinit, yinit, xt, yterm, colour),
      drawLine(
        bu,
        tu,
        bu - (xu * rad) - (yu * rad),
        tu - (yu * rad) + (xu * rad),
        "black"
      ),
      drawLine(
        bu,
        tu,
        bu - (xu * rad) + (yu * rad),
        tu - (yu * rad) - (xu * rad),
        "black"
      ),
      <text x={bt.toInt.toString} y={tt.toInt.toString}>{label}</text>
    )
  }

  def svgPlot(elems: Seq[Elem], width: Double = 1000, height: Double = 400): Elem =
    <svg version="1.1"
           baseProfile="full"
           viewBox={s"0 0 ${width.toInt} ${height.toInt}"}
           xmlns="http://www.w3.org/2000/svg">
           {elems} </svg>

  lazy val eg: Elem = svgPlot(hexagonSides())

  def plotPantsSurface(complex: PantsSurface) =
    svgPlot(allHexagonSides(complex), 150 * math.max(complex.numPants, 4), 300)


  def plotComplex(complex: TwoComplex, radius: Double = 50) : Elem = 
    svgPlot(faceRowSides(complex,radius), complex.faces.size * radius * 3 , radius * 3)

  def writeFile(text: String, fileName: String, append: Boolean = false) = {
    val writer = new FileWriter(fileName, append)
    writer.write(text)
    writer.close
  }

  val desktop = Desktop.getDesktop()

  def viewPage(content: Node) = {
    val fileName = s"image-${content.hashCode()}.html"
    val html =
      <html>
      <body>
      {content}
      </body>
      </html>
    writeFile(html.toString, fileName)
    val file = new File(fileName)
    desktop.browse(new URI("file",  file.getAbsolutePath, ""))
  }

  def apply(complex: TwoComplex, radius: Double = 50) : Unit = 
    viewPage(plotComplex(complex, radius))
}
