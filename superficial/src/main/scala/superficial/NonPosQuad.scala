package superficial

import TwoComplex._

trait NonPosQuad extends TwoComplex {nonPosQuad =>

    /**
      * Gives the designated index (1 L, 2 SL, -1 R, -2 SR, higher values by turn distance) associated to a turn
      *
      * @param e1
      * @param e2
      * @return
      */
    def turnIndex(e1: Edge, e2: Edge): Int = {
        assert(nonPosQuad.edgesEndingAt(e1.terminal).contains(e2.flip), s"$e2 cannot come after $e1 in a path")
        if (Some(e2) == nonPosQuad.turnLeft(e1)) 1
        else if (Some(e2) == nonPosQuad.slightLeft(e1)) 2
        else if (Some(e2) == nonPosQuad.turnRight(e1)) -1
        else if (Some(e2) == nonPosQuad.slightRight(e1)) -2
        else{
            val edgesLeft = nonPosQuad.vectorEdgesToTheLeftOf(e1)
            val edgesRight = nonPosQuad.vectorEdgesToTheRightOf(e1)
            if (edgesLeft.contains(e2)) (1 + edgesLeft.indexOf(e2))
            else (-1 - edgesRight.indexOf(e2))
            
        }
    }

    /**
      * Gives the succeeding edge associated to a previous edge and a turning index
      *
      * @param e
      * @param t
      */
    def turnEdge(e: Edge, t: Int): Edge = {
        assert(t!=0, "0 is not a valid index for a turn")
        if(t>0) nonPosQuad.vectorEdgesToTheLeftOf(e)(t-1)
        else nonPosQuad.vectorEdgesToTheRightOf(e)(-t-1)
    }

}

object NonPosQuad{

  def apply(twoComplex: TwoComplex): NonPosQuad = {
    assert(twoComplex.isClosedSurface)
    assert(twoComplex.faces.forall(_.sides == 4))
    assert(twoComplex.vertices.forall(twoComplex.degree(_) >= 5))
    twoComplex.asInstanceOf[NonPosQuad]
  }

}