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
              // If thisLast is same as thatLast and current._2 = true, then we
              // continue to add to current which is a path of coincidence between 
              // thisPath and thatPath. 
              // Similarly for the case thisLast != thatLast and current._2 = false. 
              val next : ((EdgePath, EdgePath), Boolean) = 
                ((Constant(thisLast.initial).+(thisLast).++(current._1._1),
                  Constant(thatLast.initial).+(thatLast).++(current._1._2)),
                  current._2)
              findHelper(thisInit, thatInit, next, accum)
            }
            // If thisLast != thatLast and current._2 = true that means current represents 
            // a path of coincidence. Because thisLast is not equal to thatLast current is
            // maximal  
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
  
  /**
   * Given two EdgePath thisPath and thatPath, outputs a vector of ((EdgePath, EdgePath, Boolean)) where 
   * each element is such that -
   * If el._2 is true then el._1._1 = el._1._2 is a path of coincidence.
   * If el._2 is false then el._1._1 and el._1._2 together constitute the boundary of a staircase.
   */
  def find (thisPath : EdgePath, thatPath : EdgePath) : Vector[((EdgePath, EdgePath), Boolean)] = {
    
    require(length(thisPath) == length(thatPath), s"$thisPath and $thatPath are not of the same length")
    require(length(thisPath) > 0, s"$thisPath is of zero length")
    require(thisPath.terminal == thatPath.terminal, 
      s"$thisPath and $thatPath do not have same terminal vertices")

    val intermediate :  Vector[((EdgePath, EdgePath), Boolean)] = thisPath match {
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
              ((Constant(thisLast.initial).+(thisLast), 
                Constant(thatLast.initial).+(thatLast)), 
               (thisLast == thatLast))
            findHelper(thisInit, thatInit, current, Vector())
          }
        }
      }
    }

    val first : ((EdgePath, EdgePath), Boolean) = intermediate.head
    val last  : ((EdgePath, EdgePath), Boolean) = intermediate.last  
    val between : Vector[((EdgePath, EdgePath), Boolean)] = intermediate.slice(1, intermediate.length - 1)

    // In case the first staircase or coincidence-path continues to the lastt one, merges them
    val result : Vector[((EdgePath, EdgePath), Boolean)] = {
      if ((first._2 == last._2) && 
          (first._1._1.terminal == last._1._1.initial) && 
          (first._1._2.terminal == last._1._2.initial)) {
        
        val newLast : ((EdgePath, EdgePath), Boolean) = 
          ((last._1._1.++(first._1._1), last._1._2.++(first._1._2)), first._2)

        between :+ newLast  
      }

      else intermediate
    }
    
    result
  }

  /**
   * == Overview ==
   * right * (left.reverse) is the boundary of the staircase.
   * Because left and right are parts of a geodesic.
   * Each face of the staircase should contain at least two edges from right * (left.reverse).
   *
   * @param left Part of the leftmost canononical geodesic
   * @param right Part of the rightmost canononical geodesic
   *
   * @return A pair. The first co-ordinate is the staircase. The second component is the set of all spokes.
   */
  def getStairCase (left : EdgePath, right : EdgePath, twoComplex : TwoComplex[Polygon]) : (TwoComplex[Polygon], Set[Edge]) = {
    require(left.initial == right.initial, s"Initial vertices of $left and $right are not same")
    require(left.terminal == right.terminal, s"Terminal vertices of $left and $right are not same")

    val boundaryOfStaircase : EdgePath = right.++(left.reverse)
    val boundaryVector : Vector[Edge] = edgeVectors(boundaryOfStaircase)
    val boundaryEdgeSet : Set[Edge]  = boundaryVector.toSet
    val facesInStairCase : Set[Polygon] = boundaryEdgeSet.flatMap(el => twoComplex.facesWithEdge(el))
    val staircase : TwoComplex[Polygon] = PureComplex(facesInStairCase)
    val spokes : Set[Edge] = staircase.edges -- (boundaryEdgeSet)
    (staircase, spokes)
  }
}  