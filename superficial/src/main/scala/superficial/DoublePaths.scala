package superficial

import TwoComplex._
import EdgePath._
import Intersection._

object DoublePaths {   
 
  /**
   * Given the two canonical geodesic cR d, 
   * outputs the set of crossing double paths in the set D+
   */ 
  def Positive (cR : EdgePath, d : EdgePath, nonPosQuad : NonPosQuad) : Set[Intersection] = {
    require(isCanonicalGeodesicLoop(cR, nonPosQuad), s"$cR is not canonical geodesic")  
    require(isCanonicalGeodesicLoop(d, nonPosQuad), s"$d is not canonical geodesic")
    // have to add check that they are primitive
    
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
  def Zero (cR : EdgePath, cL : EdgePath, d : EdgePath, nonPosQuad : NonPosQuad) : Set[Intersection] = {
    require(isCanonicalGeodesicLoop(cR, nonPosQuad), s"$cR is not canonical geodesic")
    require(isCanonicalGeodesicLoop(cL.reverse, nonPosQuad), s"inverse of $cL is not canonical geodesic")
    require(isCanonicalGeodesicLoop(d, nonPosQuad), s"$d is not canonical geodesic")
    require(cR.initial == cL.initial, s"$cR and $cL don't have the same initial vertex")

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

    val zeroIntersections = cR.intersectionsWith(d, nonPosQuad).filter(el => (el.length == 0))
    val satisfyingFirstCondition = zeroIntersections.filter(el => (condition11(el) && condition12(el)))

    satisfyingFirstCondition
  }
}