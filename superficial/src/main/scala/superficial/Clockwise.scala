package superficial

import TwoComplex._
import EdgePath._

object Clockwise {
  
  /**
   * Checks that starting from u if we take rotate by left we reach v before reaching w.
   */
  def isInOrder (u : Edge, v : Edge, w : Edge, twoComplex : TwoComplex[Polygon]) : Boolean = {

    require(twoComplex.edges.contains(u), s"$u is not a vertex of $twoComplex")
    require(twoComplex.edges.contains(v), s"$v is not a vertex of $twoComplex")
    require(twoComplex.edges.contains(w), s"$w is not a vertex of $twoComplex")
    require(Set(u,v,w).size == 3, s"$u, $v and $w are not distinct, so the method does not make much sense.")
    require(twoComplex.isClosedSurface, 
      s"The method may not work for surfaces such as $twoComplex which are not closed")
    val terminalVertices : Set[Vertex] = Set(u.terminal, v.terminal, w.terminal)
    require(terminalVertices.size == 1, s"All of the edges $u $v and $w don't have the same terminal vertex")
  

    val nextEdge : Option[Edge] = twoComplex.rotateLeftOpt(u)
    nextEdge match {
      case None => {
        System.err.println(s"rotateLeftOpt gives $None as an answer of left rotating $u." ++  
                             "Resulting failure of the method isInOrder")
        ???
      }
      case Some(ed) => {
        if (Some(ed) == Some(v)) true
        else if (Some(ed) == Some(w)) false
        else isInOrder(ed, v, w, twoComplex)
      }
    }
  }  

  /**
   * Checks if the three edges are in clockwise order. 
   * That is if we start from a and keep on taking left turns
   * we will first reach b and then c. 
   * Checks about the inputs are not given as they are already done in isInOrder
   */
  def isClockwise (a : Edge, b : Edge, c : Edge, twoComplex : TwoComplex[Polygon]) : Boolean = {
    
    val condition1 : Boolean = isInOrder(a, b, c, twoComplex)
    val condition2 : Boolean = isInOrder(b, c, a, twoComplex)
    val condition3 : Boolean = isInOrder(c, a, b, twoComplex)

    (condition1 && condition2 && condition3)  
  }
}