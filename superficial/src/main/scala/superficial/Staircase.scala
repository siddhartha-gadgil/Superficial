package superficial

import Polygon.Index
import scala.collection.immutable.Nil
import superficial.Generator.a
import TwoComplex._
import EdgePath._

object Staircase {

  def findHelper (thisPath : EdgePath, thatPath : EdgePath, current : ((EdgePath, EdgePath), Boolean), 
    accum : Vector[((EdgePath, EdgePath), Boolean)]) : Vector[((EdgePath, EdgePath), Boolean)] = {
    thisPath match {
      case Constant(u) => accum :+ current
      case Append(thisInit, thisLast) => {
        thatPath match {
          case Constant(v) => {
            System.err.println("This case in findHelper shouldn't be reached" ++ 
              " given the requirement of the method find in the object staircase")
            ???
          }
          case Append(thatInit, thatLast) => {
            if ((thisLast == thatLast) == current._2) {
              val next : ((EdgePath, EdgePath), Boolean) = 
                ((Constant(thisLast.initial).+(thisLast).++(current._1._1),
                  Constant(thatLast.initial).+(thatLast).++(current._1._2)),
                  current._2)
              findHelper(thisInit, thatInit, next, accum)
            }  
            else {
              val next : ((EdgePath, EdgePath), Boolean) =
                ((Constant(thisLast.initial).+(thisLast), 
                  Constant(thatLast.initial).+(thatLast)), !current._2)

              findHelper(thisInit, thatInit, next, accum :+ current)
            }
          }  
        }
      }
    }
  }

  def find (thisPath : EdgePath, thatPath : EdgePath) : Vector[((EdgePath, EdgePath), Boolean)] = {
    
    require(length(thisPath) == length(thatPath), s"$thisPath and $thatPath are not of the same length")
    require(length(thisPath) > 0, s"$thisPath is of zero length")
    require(thisPath.terminal == thatPath.terminal, 
      s"$thisPath and $thatPath do not have same terminal vertices")

    thisPath match {
      case Constant(v) => {
        System.err.println("This case in findHelper shouldn't be reached" ++ 
              " given the requirement of the method find in the object staircase")
        ???
      }

      case Append(thisInit, thisLast) => {
        thatPath match {
          case Constant(u) => {
            System.err.println("This case in findHelper shouldn't be reached" ++ 
              " given the requirement of the method find in the object staircase")
            ??? 
          }
          
          case Append(thatInit, thatLast) => {
            val current : ((EdgePath, EdgePath), Boolean) = 
              ((Constant(thisLast.initial), Constant(thatLast.initial)), 
               (thisLast == thatLast))
            findHelper(thisInit, thatInit, current, Vector())
          }
        }
      }
    }
  }
}  