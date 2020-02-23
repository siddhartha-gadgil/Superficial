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

trait EquivalenceClass { equivalenceClass =>
  val sets : Set[Set[Any]]

  def expandWith(newSet : Set[Any]) : EquivalenceClass = {
    val destination = equivalenceClass.sets.find(ss => ss.intersect(newSet).nonEmpty)
    destination match {
      case None => EquivalenceClass.apply(equivalenceClass.sets.+(newSet))
      case Some(element) => {
        val newElement = element.++(newSet)
        EquivalenceClass.apply(equivalenceClass.sets.-(element).+(newElement))
      }
    } 
  }

  /**
   * Checks if the sets in the equivalence class are mutually disjoint
   */
  def isWellDefined = {
    val allElements : Set[Any] = equivalenceClass.sets.flatMap(e => e)
    (allElements.filter(el => 
      (equivalenceClass.sets.filter(_.contains(el)).size != 1)).size == 0)
  }  
}

object EquivalenceClass {
  
  def apply (newSets : Set[Set[Any]]) : EquivalenceClass = new EquivalenceClass {
    val sets = newSets
  }
}