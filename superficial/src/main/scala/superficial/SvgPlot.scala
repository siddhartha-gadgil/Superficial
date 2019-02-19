package superficial
import scala.xml._

object SvgPlot{
    def line(x1: Double, y1: Double, x2: Double, y2: Double, colour: String) = 
        <line x1={x1.toString} x2={x2.toString} y1={y1.toString} y2={y2.toString} stroke={colour} stroke-width="1" xmlns="http://www.w3.org/2000/svg"></line>
}