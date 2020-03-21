package superficial

import EdgePath._
import NonPosQuad._

trait Intersection { intersection =>
  val start : (Int, Int)
  val end : (Int, Int)
  val turnBefore : Int
  val turnAfter : Int

  def mod(m : Int, n : Int) : Int = ((m % n) + n) % n

  /** 
   *Checks that the intersection is a valid intersection between 
   *thisPath and thatPath. For now it does not check turns.
   */
  def isValidBetween (thisPath : EdgePath, thatPath : EdgePath, twoComplex : TwoComplex) : Boolean = {
    val inThis : EdgePath = thisPath.cyclicalTake(intersection.start._1, intersection.end._1)
    val inThat : EdgePath = thatPath.cyclicalTake(intersection.start._2, intersection.end._2)
    val condition1 : Boolean = inThis == inThat
    val thisLength : Int = EdgePath.length(thisPath)
    val thatLength : Int = EdgePath.length(thatPath)
    val i1 : Int = mod(intersection.start._1 - 1, thisLength)
    val i2 : Int = mod(intersection.start._2 - 1, thatLength)
    val thisVect : Vector[Edge] = EdgePath.edgeVectors(thisPath)
    val thatVect : Vector[Edge] = EdgePath.edgeVectors(thatPath)
    val condition2 : Boolean = true //(twoComplex.angleBetween(thisVect(i1), thatVect(i2)) == intersection.turnBefore)
    val condition3 : Boolean = true
      //(twoComplex.angleBetween(thisVect(intersection.end._1).flip, thatVect(intersection.end._2).flip) == intersection.turnAfter)
    
    (condition1 && condition2 && condition3)
  } 

  def isMergableWith(other : Intersection, thisLimit : Int, thatLimit : Int) : Boolean = {
    val condition1 : Boolean = (mod(intersection.end._1 + 1, thisLimit) == other.start._1)
    val condition2 : Boolean = (mod(intersection.end._2 + 1, thatLimit) == other.start._2)
    val condition3 : Boolean = (intersection.turnAfter == 0)
    val condition4 : Boolean = (other.turnBefore == 0)
    (condition1 && condition2 && condition3 && condition4)
  }

  def mergeWith(other : Intersection, thisLimit : Int, thatLimit : Int) : Intersection = {
    require(intersection.isMergableWith(other, thisLimit, thatLimit), s"$intersection is not mergable with $other")
    Intersection.apply(intersection.start, other.end, intersection.turnBefore, other.turnAfter)
  }

  def findMergable(inside : Vector[Intersection], thisLimit : Int, thatLimit : Int) : Option[Intersection] = 
    inside.find(el => intersection.isMergableWith(el, thisLimit, thatLimit))

  def getEdgePathWithSigns(thisPath : EdgePath, thatPath : EdgePath, twoComplex : TwoComplex) : (EdgePath, Int) = {
    require(intersection.isValidBetween(thisPath, thatPath, twoComplex), 
      s"$intersection is not a valid intersection between $thisPath and $thatPath")
    val sign : Int = {
      if ((intersection.turnBefore > 0) && (intersection.turnAfter > 0)) 1
      else if ((intersection.turnBefore < 0) && (intersection.turnAfter < 0)) -1
      else 0
    }
    val newPath : EdgePath = thisPath.cyclicalTake(intersection.start._1, intersection.end._1)
    (newPath, sign)
  }

  def getSign(thisPath : EdgePath, thatPath : EdgePath, twoComplex : TwoComplex): Int = {
    require(intersection.isValidBetween(thisPath, thatPath, twoComplex), 
      s"$intersection is not a valid intersection between $thisPath and $thatPath")
    val sign : Int = {
      if ((intersection.turnBefore < 0) && (intersection.turnAfter < 0)) 1
      else if ((intersection.turnBefore > 0) && (intersection.turnAfter > 0)) -1
      else 0
    }
    sign
  }

  def isCrossing(thisPath : EdgePath, thatPath : EdgePath, twoComplex : TwoComplex): Int = {
    require(intersection.isValidBetween(thisPath, thatPath, twoComplex), 
      s"$intersection is not a valid intersection between $thisPath and $thatPath")
    ???

  }

}

object Intersection {

  final case class InterCons(newStart : (Int, Int), newEnd : (Int, Int), newTurnBefore : Int, newTurnAfter : Int) 
    extends Intersection {
    val start : (Int, Int) = newStart
    val end : (Int, Int) = newEnd
    val turnBefore : Int = newTurnBefore
    val turnAfter : Int = newTurnAfter 
  }

  def apply(newStart : (Int, Int), newEnd : (Int, Int), newTurnBefore : Int, newTurnAfter : Int) : Intersection 
    = InterCons(newStart, newEnd, newTurnBefore, newTurnAfter) 

  def findMergablePair (inside : Set[Intersection], thisLimit : Int, thatLimit : Int) : Option[(Intersection, Intersection)] = {
    def helper (oneVect : Vector[Intersection], otherVect : Vector[Intersection]) : Option[(Intersection, Intersection)]= {
      oneVect match {
        case (el +: els) => {
          el.findMergable(otherVect, thisLimit, thatLimit) match {
            case None => helper(els, otherVect)
            case Some(fl) => Some((el, fl))
          }
        }
        case _ => None
      }
    }
    val insideVect : Vector[Intersection] = inside.toVector
    helper(insideVect, insideVect)
  }
  
  def mergeAll (allInter : Vector[Intersection], thisLimit : Int, thatLimit : Int) : Vector[Intersection] = {
    val interPair : Option[(Intersection, Intersection)] = Intersection.findMergablePair(allInter.toSet, thisLimit, thatLimit)
    interPair match {
      case None => allInter
      case Some((el, fl)) => {
        val gl : Intersection = el.mergeWith(fl, thisLimit, thatLimit)
        if ((el == fl) && (fl == gl)) mergeAll(allInter.toSet.-(el).toVector, thisLimit, thatLimit) :+ el
        else {
          val newInter : Vector[Intersection] = allInter.toSet.-(el).-(fl).+(gl).toVector
          mergeAll(newInter, thisLimit, thatLimit)
        }
      }
    }    
  }
} 