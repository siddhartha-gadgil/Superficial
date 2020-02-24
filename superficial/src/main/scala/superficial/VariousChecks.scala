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
  val sets : Set[Set[EdgePath]]

  def expandWith(newSet : Set[EdgePath  ]) : EquivalenceClass = {
    val destination = equivalenceClass.sets.find(ss => ss.intersect(newSet).nonEmpty)
    val intermediate = destination match {
      case None => EquivalenceClass.apply(equivalenceClass.sets.+(newSet))
      case Some(element) => {
        val newElement = element.++(newSet)
        EquivalenceClass.dumbApply(equivalenceClass.sets.-(element).+(newElement))
      }
    }
    val result = intermediate.makeWellDefined
    assert(result.isWellDefined, 
      s"The result $result of expanding the EquivalenceClass $equivalenceClass with $newSet" ++ 
       "is not well defined as a equivalence class")
    result 
  }

  /**
   * Checks if the sets in the equivalence class are mutually disjoint.
   * We do not use the findIntersectingPair method to check so that we
   * can check methods where findIntersectingPairs are used
   */
  def isWellDefined = {
    val allElements : Set[EdgePath] = equivalenceClass.sets.flatMap(e => e)
    (allElements.filter(el => 
      (equivalenceClass.sets.filter(_.contains(el)).size != 1)).size == 0)
  }

  def findIntersectingPair : Option[(Set[EdgePath], Set[EdgePath])] = {
    val setList : List[Set[EdgePath]] = equivalenceClass.sets.toList
    def helper(oneList : List[Set[EdgePath]], anotherList : List[Set[EdgePath]]) : Option[(Set[EdgePath], Set[EdgePath])] = {
      oneList match {
        case Nil => None
        case el :: els => {
          anotherList match {
            case Nil => helper(els, els)
            case fl :: fls => {
              if ((el.intersect(fl).nonEmpty) && (el != fl)) Some((el, fl))
              else helper(el :: els, fls)
            }
          }
        }
      }
    }
    helper(setList, setList)
  }

  /** 
   * If the equivalence class is not well defined makes it well defined. 
   * That is merges intersecting 
   */
  def makeWellDefined : EquivalenceClass = {
    val result = equivalenceClass.findIntersectingPair match {
      case None => equivalenceClass
      case Some((a, b)) => {
        val newSets : Set[Set[EdgePath]] = equivalenceClass.sets.-(a).-(b).+(a.++(b))
        EquivalenceClass.dumbApply(newSets).makeWellDefined
      }
    }
    assert(result.isWellDefined, s"The result $result of makeWellDefined is not a collection of" ++ 
      "mutually disjoint sets")
    result  
  }
}

object EquivalenceClass {
  
  def dumbApply (newSets : Set[Set[EdgePath]]) : EquivalenceClass = new EquivalenceClass {
    val sets = newSets
  }

  def apply (newSets : Set[Set[EdgePath]]) : EquivalenceClass = {
    val intermediate : EquivalenceClass = EquivalenceClass.dumbApply(newSets)
    val result : EquivalenceClass = intermediate.makeWellDefined
    result
  }
}

trait HomotopyClassOfPaths { homotopyClassOfPaths =>
  val initial : Vertex
  val terminal : Vertex
  val homotopyClasses : EquivalenceClass
}

object HomotopyClassOfPaths {  
  def apply (initialV : Vertex, terminalV : Vertex, classes : Set[Set[EdgePath]], twoComplex : TwoComplex) = {
    val allElements : Set[EdgePath] = classes.flatMap(e => e)
    assert(allElements.forall(_.inTwoComplex(twoComplex)), s"All EdgePaths are not inside the TwoComplex $twoComplex")
    assert(allElements.forall(_.initial == initialV), s"All EdgePaths do not start at $initialV")
    assert(allElements.forall(_.terminal == terminalV), s"All EdgePaths do not start at $terminalV")

    new HomotopyClassOfPaths { 
      val initial : Vertex = initialV
      val terminal  : Vertex = terminalV
      val homotopyClasses : EquivalenceClass = EquivalenceClass.apply(classes)
    }
  }

  def starter(twoComplex : TwoComplex) : HomotopyClassOfPaths = {
    val faceList : List[Polygon] = twoComplex.faces.toList
   
    // while running this we have to ensure that i <= j. Also that i, j >= 0 
    def helper (face : Polygon, i : Int, j : Int) : Set[EdgePath] = {
      require(i <= j, s"$i is not less than or equal to $j in helper method of starter method")
      val limitL = face.boundary.length
      
      if(i == 0 && j == 0) Set(Constant(face.boundary.head.initial), EdgePath.apply(face.boundary))
      else ???
    }
    ??? 
  }
}