package superficial

import org.scalajs.dom
import org.scalajs.dom._
import org.scalajs.dom.html.Input
import scalatags.JsDom.all._

//import scala.scalajs.js.JSApp
import scala.scalajs.js.annotation._


@JSExportTopLevel("Superficial")
object Web{
  @JSExport
  def main(): Unit = {

    val jsDiv = document.getElementById("js-div")
    jsDiv.appendChild(p("time to be superficial").render)
  }
}
