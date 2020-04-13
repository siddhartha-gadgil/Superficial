package superficial

import TwoComplex._
import EdgePath._
import Intersection._

object DoublePaths {   
 
  /**
   * Given the two canonical geodesic cR d, 
   * outputs the set of crossing double paths of positive length.
   */ 
  def Positive (cR : EdgePath, d : EdgePath, nonPosQuad : NonPosQuad) : Set[Intersection] = {
    require(isCanonicalGeodesicLoop(cR, nonPosQuad), s"$cR is not canonical geodesic")  
    require(isCanonicalGeodesicLoop(d, nonPosQuad), s"$d is not canonical geodesic")
    
    val positiveIntersections = cR.intersectionsWith(d, nonPosQuad).filter(el => 
      ((el.length >= 1) && (el.isCrossing(cR,d,nonPosQuad))))
      
    positiveIntersections  
  } 
}