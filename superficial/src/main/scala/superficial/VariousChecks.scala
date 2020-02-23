package superficial

import Polygon.Index
import scala.collection.immutable.Nil
import superficial.Generator.a
import TwoComplex._
import EdgePath._
import Quadrangulation._

object CheckQuadrangulation {

  def truePredicate (edgePath : EdgePath) : Boolean = true

  def Termination (twoComplex : TwoComplex, n : Int) : Boolean = {
    val quadWithMaps = Quadrangulation.quadrangulate(twoComplex)
    val quad = quadWithMaps._1
    def forwardMap = quadWithMaps._2._1
    def backwardMap = quadWithMaps._2._2
    val somePathsInOriginal = EdgePath.enumerate(twoComplex, n, truePredicate).toList
    val somePathsInQuad = somePathsInOriginal.map(forwardMap(_))
    def isGoodForBackwardMap (edgePath : EdgePath) : Boolean = {
      (edgePath.inTwoComplex(quad) && 
      (edgePath.isLoop || 
      (twoComplex.vertices.contains(edgePath.initial) && (twoComplex.vertices.contains(edgePath.terminal)))))
    }
    val someOtherPathsInQuad = EdgePath.enumerate(quad, n, isGoodForBackwardMap).toList
    val someOtherPathsInOriginal = someOtherPathsInQuad.map(backwardMap(_))
 
    (somePathsInOriginal.length == somePathsInQuad.length && 
     someOtherPathsInOriginal.length == someOtherPathsInQuad.length)
  }
}