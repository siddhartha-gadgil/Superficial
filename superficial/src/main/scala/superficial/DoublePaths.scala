package superficial

import TwoComplex._
import EdgePath._
import Intersection._
import Staircase._

object DoublePaths {   
 
  /**
   * Given the two canonical geodesic cR d, 
   * outputs the set of crossing double paths in the set D+
   */ 
  def Positive (cR : EdgePath, d : EdgePath, nonPosQuad : NonPosQuad) : Set[Intersection] = {
    require(cR.isPrimitiveLoop, s"$cR is not a primitive loop")
    require(d.isPrimitiveLoop, s"$d is not a primitive loop")
    require(isCanonicalGeodesicLoop(cR, nonPosQuad), s"$cR is not canonical geodesic")  
    require(isCanonicalGeodesicLoop(d, nonPosQuad), s"$d is not canonical geodesic")
     
    val positiveIntersections = cR.intersectionsWith(d, nonPosQuad).filter(el => 
      ((el.length >= 1) && (el.isCrossing(cR,d,nonPosQuad))))

    positiveIntersections  
  } 

  def mod(m : Int, n : Int) : Int = ((m % n) + n) % n 

  /**
   * Given the left and rightmost geodesic cL and cR of c, 
   * and another canonical geodesic d,
   * gives the the set of crossing double paths in the set D0
   */ 
  def Zero (cL : EdgePath, cR : EdgePath, d : EdgePath, nonPosQuad : NonPosQuad) : Set[Intersection] = {
    require(cL.isPrimitiveLoop, s"$cL is not a primitive loop")
    require(cR.isPrimitiveLoop, s"$cR is not a primitive loop")
    require(d.isPrimitiveLoop, s"$d is not a primitive loop")
    require(isCanonicalGeodesicLoop(cR, nonPosQuad), s"$cR is not canonical geodesic")
    require(isCanonicalGeodesicLoop(cL.reverse, nonPosQuad), s"inverse of $cL is not canonical geodesic")
    require(isCanonicalGeodesicLoop(d, nonPosQuad), s"$d is not canonical geodesic")
    require(cR.initial == cL.initial, s"$cR and $cL don't have the same initial vertex")
    
    val spokes : Set[Edge] = getStairCase(cL, cR, nonPosQuad)._2

    /// cL(i) = cR(i)
    def condition11 (intersection : Intersection) : Boolean = {
      edgeVectors(cL)(intersection.start._1).initial == 
      edgeVectors(cR)(intersection.start._1).initial
    }

    def condition12 (intersection : Intersection) : Boolean = {
      val cLEdgeBefore : Edge = edgeVectors(cL)(mod(intersection.start._1 - 1, length(cL)))
      val dEdgeBefore  : Edge = edgeVectors(d)(mod(intersection.start._2 - 1, length(d)))
      val cLEdgeAfter  : Edge = edgeVectors(cL)(intersection.start._1) 
      val dEdgeAfter   : Edge = edgeVectors(d)(intersection.start._2)

      ((cLEdgeBefore == dEdgeBefore) || 
       (cLEdgeAfter  == dEdgeAfter))
    }

    def condition21 (intersection : Intersection) : Boolean = {
      val dEdgeBefore    : Edge = edgeVectors(d)(mod(intersection.start._2 - 1, length(d)))
      val dEdgeAfter     : Edge = edgeVectors(d)(intersection.start._2)
      val endOfSpokeInCL : Option[Int] = cL.findVertexIndex(dEdgeBefore.initial)
      endOfSpokeInCL match {
        case None => false
        case Some(indexInCL) => {
          val cLEdgeBefore    : Edge = edgeVectors(cL)(mod(indexInCL - 1, length(cL)))
          val dTwoEdgeBefore  : Edge = edgeVectors(d)(mod(intersection.start._2 - 2, length(d)))
          val cREdge          : Edge = edgeVectors(cR)(intersection.start._1)
          ((cLEdgeBefore == dTwoEdgeBefore) &&
           // spoke condtion
           (dEdgeBefore.initial  == edgeVectors(cL)(indexInCL).initial) &&
           (dEdgeBefore.terminal == edgeVectors(cR)(intersection.start._1).initial) && 
           (spokes.contains(dEdgeBefore))) 
        }
      }
    }

    def condition22 (intersection : Intersection) : Boolean = {
      val dEdgeAfter     : Edge = edgeVectors(d)(intersection.start._2)
      val dTwoEdgeAfter  : Edge = edgeVectors(d)(mod(intersection.start._2 + 1, length(d)))
      val endOfSpokeInCL : Option[Int] = cL.findVertexIndex(dEdgeAfter.terminal)
      endOfSpokeInCL match {
        case None => false
        case Some(indexInCL) => {
          val cLEdgeAfter  : Edge = edgeVectors(cL)(indexInCL)  
          (dTwoEdgeAfter == cLEdgeAfter)
          // spoke condtion
          (dEdgeAfter.initial  == edgeVectors(cR)(intersection.start._1).initial) &&
          (dEdgeAfter.terminal == edgeVectors(cL)(indexInCL).initial) && 
          (spokes.contains(dEdgeAfter))
        }
      }  
    }

    val zeroIntersections         : Set[Intersection] = 
      cR.intersectionsWith(d, nonPosQuad).filter(el => (el.length == 0))
    val satisfyingFirstCondition  : Set[Intersection] = 
      zeroIntersections.filter(el => (condition11(el) && condition12(el)))
    val satisfyingSecondCondition : Set[Intersection] = 
      zeroIntersections.filter(el => (condition21(el) || condition22(el)))

    (satisfyingFirstCondition ++ satisfyingSecondCondition)
  }
}