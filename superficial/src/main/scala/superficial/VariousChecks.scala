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

  def merge (anotherClass : EquivalenceClass) : EquivalenceClass = {
    val newClass : Set[Set[EdgePath]] = equivalenceClass.sets.++(anotherClass.sets)
    EquivalenceClass.apply(newClass)
  } 
}

object EquivalenceClass {
  
  def dumbApply (newSets : Set[Set[EdgePath]]) : EquivalenceClass = new EquivalenceClass {
    val sets = newSets
  }

  def apply (newSets : Set[Set[EdgePath]]) : EquivalenceClass = {
    val intermediate : EquivalenceClass = EquivalenceClass.dumbApply(newSets)
    val result : EquivalenceClass = intermediate.makeWellDefined
    assert(result.isWellDefined, s"The result $result of makeWellDefined is not a collection of" ++ 
      "mutually disjoint sets")
    result
  }
}

trait HomotopyClassesOfPaths { homotopyClassesOfPaths =>
  val initial : Vertex
  val terminal : Vertex
  val homotopyClasses : EquivalenceClass

  def isWellDefined : Boolean = {
    val allElements : Set[EdgePath] = homotopyClasses.sets.flatMap(e => e)
    allElements.forall(el => (el.initial == initial) && (el.terminal == terminal))
  }

  def expandWith (toAdd : Set[EdgePath]): HomotopyClassesOfPaths = {
    require(toAdd.forall(_.initial == homotopyClassesOfPaths.initial), 
      s"All EdgePaths do not start at ${homotopyClassesOfPaths.initial}")
    require(toAdd.forall(_.terminal == homotopyClassesOfPaths.terminal), 
      s"All EdgePaths do not start at ${homotopyClassesOfPaths.terminal}")

    val newClass : EquivalenceClass = homotopyClassesOfPaths.homotopyClasses.expandWith(toAdd)
    HomotopyClassesOfPaths.apply(homotopyClassesOfPaths.initial, homotopyClassesOfPaths.terminal, newClass.sets)   
  }

  def merge (anotherClass : HomotopyClassesOfPaths) : HomotopyClassesOfPaths = {
    require(anotherClass.initial == homotopyClassesOfPaths.initial, 
      s"Initial vertices of $anotherClass and $homotopyClassesOfPaths are not same")
    require(anotherClass.terminal == homotopyClassesOfPaths.terminal, 
      s"Terminal vertices of $anotherClass and $homotopyClassesOfPaths are not same")

    val newClass : EquivalenceClass = homotopyClassesOfPaths.homotopyClasses.merge(anotherClass.homotopyClasses)
    val result = HomotopyClassesOfPaths.apply(homotopyClassesOfPaths.initial, homotopyClassesOfPaths.terminal, newClass.sets)
    assert(result.isWellDefined, s"The result $result is not well-defined")
    result
  }

  def multiply (anotherClass : HomotopyClassesOfPaths) : HomotopyClassesOfPaths = {
    def helper(aSet : Set[EdgePath], bSet : Set[EdgePath]) : Set[EdgePath] = {
      val aList : List[EdgePath] = aSet.toList
      val bList : List[EdgePath] = bSet.toList
      val product : List[EdgePath] = aList.flatMap(a => bList.map(b => a.++(b)))
      product.toSet 
    }

    val thisList : List[Set[EdgePath]] = homotopyClassesOfPaths.homotopyClasses.sets.toList
    val anotherList : List[Set[EdgePath]] = anotherClass.homotopyClasses.sets.toList
    val product : List[Set[EdgePath]] = thisList.flatMap(a => anotherList.map(b => helper(a,b)))
    val newClasses : Set[Set[EdgePath]] = product.toSet
    HomotopyClassesOfPaths.apply(homotopyClassesOfPaths.initial, anotherClass.terminal, newClasses)
  }
}

object HomotopyClassesOfPaths {  
  def apply (initialV : Vertex, terminalV : Vertex, classes : Set[Set[EdgePath]]) = {
    val allElements : Set[EdgePath] = classes.flatMap(e => e)
    assert(allElements.forall(_.initial == initialV), s"All EdgePaths do not start at $initialV")
    assert(allElements.forall(_.terminal == terminalV), s"All EdgePaths do not start at $terminalV")

    new HomotopyClassesOfPaths { 
      val initial : Vertex = initialV
      val terminal  : Vertex = terminalV
      val homotopyClasses : EquivalenceClass = EquivalenceClass.apply(classes)
    }
  }
 
  /**
   *Given a face and indices gives a set containing two possible paths between same intial and terminal vertices. 
   */
  def starter (face : Polygon, i : Int, j : Int) : HomotopyClassesOfPaths = {
    require(i <= j, s"$i is not less than or equal to $j in helper method of starter method")
    require((i >= 0) && (i < face.boundary.length), s"$i is not inside [1, ${face.boundary.length - 1}]")
    require((j >= 0) && (j < face.boundary.length), s"$j is not inside [1, ${face.boundary.length - 1}]")
    val limitL = face.boundary.length
      
    val newClass = {
      if(i == 0 && j == 0) {
        Set(Constant(face.boundary.head.initial), EdgePath.apply(face.boundary))
      }  
      else if (i == 0 && (j == (face.boundary.length - 1))) {
        Set(Constant(face.boundary.last.terminal), EdgePath.apply(face.boundary))
      }  
      else if ((i == (face.boundary.length - 1) && (j == face.boundary.length - 1))) {
        Set(Constant(face.boundary.last.terminal), EdgePath.apply(face.boundary))
      }
      else if (i == j) {
        Set(Constant(face.boundary(i).initial), 
        EdgePath.apply(face.boundary.slice(0, i)).reverse.++
        (EdgePath.apply(face.boundary.slice(i, face.boundary.length)).reverse))
      }
      else {
        Set(EdgePath.apply(face.boundary.slice(i, j)), 
        EdgePath.apply(face.boundary.slice(0, i)).reverse.++
        (EdgePath.apply(face.boundary.slice(j, face.boundary.length)).reverse))
      }
    }
  HomotopyClassesOfPaths.apply(newClass.head.initial, newClass.head.terminal, Set(newClass))     
  }
}

trait CollectionOfHomotopyClasses { collection =>
  val classes : Set[HomotopyClassesOfPaths]

  def expandWith (newClasses : HomotopyClassesOfPaths) : CollectionOfHomotopyClasses = {
    val classToMerge = collection.classes.find(cl => (cl.initial == newClasses.initial && cl.terminal == newClasses.terminal))
    classToMerge match {
      case None => CollectionOfHomotopyClasses.apply(collection.classes.+(newClasses))
      case Some(cl) => CollectionOfHomotopyClasses.apply(collection.classes.-(cl).+(cl.merge(newClasses)))
    }
  } 

  def isWellDefined : Boolean = {
    val classList = classes.toList
    val terminalPoints = classList.map(e => (e.initial, e.terminal))
    val condition1 : Boolean = terminalPoints.distinct.size == terminalPoints.length
    val condition2 : Boolean = classList.forall(e => e.homotopyClasses.isWellDefined)
    (condition1 && condition2)
  }

  def makeWellDefined : CollectionOfHomotopyClasses = {
    val classList = classes.toList
    def findToMerge(oneList : List[HomotopyClassesOfPaths], anotherList : List[HomotopyClassesOfPaths]) : 
    Option[(HomotopyClassesOfPaths, HomotopyClassesOfPaths)] = {
      oneList match {
        case Nil => None
        case el :: els => {
          anotherList match {
            case Nil => findToMerge (els, els)
            case fl :: fls => {
              if ((el.initial == fl.initial) && (el.terminal == fl.terminal) && (el != fl)) Some((el, fl))
              else findToMerge(el :: els, fls)
            }
          }
        }
      }
    } 
    val toMerge = findToMerge(classList, classList)
    val result = toMerge match {
      case None => collection
      case Some((el, fl)) => {
        CollectionOfHomotopyClasses.dumbApply(collection.classes.-(el).-(fl).+(el.merge(fl))).makeWellDefined
      }  
    }
    assert(result.isWellDefined, s"Result $result of makeWellDefined for CollectionOfHomotopyClasses is not well defined")
    result
  } 

  def mainCourse : CollectionOfHomotopyClasses = {
    val classVector : Vector[HomotopyClassesOfPaths] = collection.classes.toVector
    def helper (oneClass : Vector[HomotopyClassesOfPaths], anotherClass : Vector[HomotopyClassesOfPaths], 
      accum : CollectionOfHomotopyClasses) : CollectionOfHomotopyClasses = {
        oneClass match {
          case cl +: cls => {
            anotherClass match {
              case dl +: dls => {
                if (cl.terminal == dl.initial) helper(cl +: cls, dls, accum.expandWith(cl.multiply(dl)))
                else helper(cl +: cls, dls, accum)
              }
              case _ => helper(cls, cls, accum)
            }
          }
          case _ => accum
        }
      } 
    helper(classVector, classVector, CollectionOfHomotopyClasses.apply(Set()))  
  } 
}

object CollectionOfHomotopyClasses {

  def dumbApply (newClasses : Set[HomotopyClassesOfPaths]) : CollectionOfHomotopyClasses = {
    new CollectionOfHomotopyClasses { 
      val classes = newClasses
    }
  }

  def apply (newClasses : Set[HomotopyClassesOfPaths]) : CollectionOfHomotopyClasses = {
    CollectionOfHomotopyClasses.dumbApply(newClasses).makeWellDefined
  }

  def starter (face : Polygon) : CollectionOfHomotopyClasses = {
    val limitL : Int = face.boundary.length 
    val allIndices : Vector[Int] = (0 to (limitL - 1)).toVector
    val facesWithIndices : Vector[(Polygon, (Int, Int))] = 
      allIndices.flatMap(i => (allIndices.map(j => (face, (i, j))))).filter(el => (el._2._1 < el._2._2))
    val newClasses : Set[HomotopyClassesOfPaths] = 
      facesWithIndices.map(el => HomotopyClassesOfPaths.starter(el._1, el._2._1, el._2._2)).toSet
    CollectionOfHomotopyClasses.apply(newClasses)
  }

  def starter (twoComplex : TwoComplex) : CollectionOfHomotopyClasses = {
    val newClasses : Set[HomotopyClassesOfPaths] = 
      twoComplex.faces.flatMap(fc => CollectionOfHomotopyClasses.starter(fc).classes)
    CollectionOfHomotopyClasses.apply(newClasses)  
  }
}
