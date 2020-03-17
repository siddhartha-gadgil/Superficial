package superficial

import Polygon.Index
import scala.collection.immutable.Nil
import superficial.Generator.a
import TwoComplex._
import EdgePath._

/**
 *a quadrilateral is a polygon with four edges.
 */
trait Quadrilateral extends Polygon {
    val sides: Int = 4
}

trait Quadrangulation extends TwoComplex {

}

object Quadrangulation {

  def isQuadrangulation (twoComplex : TwoComplex) : Boolean = {
    twoComplex.faces.forall(f => (f.boundary.length == 4))
  }

  def apply(twoComplex: TwoComplex): Quadrangulation = {
        assert(twoComplex.isClosedSurface)
        assert(twoComplex.faces.forall(_.sides == 4))
        twoComplex.asInstanceOf[Quadrangulation]
    }

  case class BaryCenter(face: Polygon) extends Vertex

  // case class QuadEdge(face: Polygon, index: Int) extends EdgePair(BaryCenter(face), face.boundary(index).terminal)
  case class QuadEdge(face: Polygon, index: Int) extends EdgePair(BaryCenter(face), face.boundary(index).initial)

  // The `bdy` parameter is redundant, but will need significant refactoring to avoid
  case class QuadFace(face: Polygon, flipFace: Polygon, indexOfEdge: Int, indexOfFlip: Int) extends Polygon{
      val sides: Int = 4
      val boundary: Vector[Edge] = 
        Vector(
        //QuadEdge(face, indexOfEdge).Positive, // from barycenter of face of edge to edge.terminal
        QuadEdge(face, mod(indexOfEdge + 1, face.boundary.length)).Positive,
        //QuadEdge(flipFace, mod(indexOfFlip - 1, flipFace.boundary.length)).Negative, // from edge.terminal to barycenter of face of edge.flip
        QuadEdge(flipFace, indexOfFlip).Negative,
        //QuadEdge(flipFace, indexOfFlip).Positive, // from barycenter of face of edge.flip to edge.initial
        QuadEdge(flipFace, mod(indexOfFlip + 1, flipFace.boundary.length)).Positive,
        //QuadEdge(face, mod(indexOfEdge - 1, face.boundary.length)).Negative // from edge.intial to barycenter of face of edge
        QuadEdge(face, indexOfEdge).Negative
      )
      val vertices: Set[Vertex] = boundary.map(_.initial).toSet
  }

   def mod (m : Int, n : Int) = ((m % n) + n) % n 


  /**
   *Gives the quadrangulation of a twocomplex along maps from edgepaths from the twocomplex to  
   *paths in the quadragulation.
   *The forWard EdgePath method turns every edge in the original twoComplex into an EdgePath of length two 
   *in the corresponding face in the quadrangulation.  
   *The backWard EdgePath method only works if the EdgePath in the quadrangulation
   *starts and ends at pre existing vertices, or is a loop on a barycenter of length more than one.
   *In the first case it returns a fixed end point homotopic path in the original TwoComplex.
   *In the second case if returns a freely homotopic path in the original TwoComplex. 
   *In the first case the backWard EdgePath map considers edges in the quadragulation in Pairs. 
   *Because the pre existing vertices and the barycenters form a bipartite set in the quadrangulation, a consequtive
   *pair of edges in the quadragulation (say u and v), starting from a (which is a pre existing vertex), 
   *passing through b (which is a barycenter) and ending at c (which is a pre existing vertex) is homotopic
   *to a sequence of edges in the face corresponding to b. So we can take such a path.
   *In the second case because the pre existing vertices and the barycenters form a bipartite set in the quadrangulation,
   *shifting the basepoint makes the loop start and end at a pre existing vertex.
   */
  def quadrangulate (twoComplex : TwoComplex) : (TwoComplex, (EdgePath => EdgePath, EdgePath => EdgePath)) = {
 
    require(twoComplex.isClosedSurface, "Algorithm only works for closed surfaces")       

    val faceList = twoComplex.faces.toList
    val facesWithIndexes :  List[(Polygon, Int)] = faceList.flatMap(f => ((0 to (f.boundary.length - 1)).map(ind => (f, ind))))

    // def createBarycenter (face : Polygon) : Vertex = BaryCenter(face)

    val barycentersList = faceList.map(BaryCenter(_))
    val barycenters = faceList.zip(barycentersList).toMap

    // def createEdgePairs (face : Polygon, index : Int) : EdgePair = QuadEdge(face, index)

    val newEdgeMap1 : Map[(Polygon, Int),EdgePair] = 
      facesWithIndexes.map{
        case (poly, j) => (poly, j) -> QuadEdge(poly, j)
      }.toMap

    
  
    // A new edge from edge.terminal to barycenter is mapped to the pair (left, right) where
    // left and right are edges in the original twoComplex which are immediately left or right to
    // the new edge
    val reverseEdgeMap : Map[Edge, (Edge, Edge)] = {
      val intermediate = newEdgeMap1.toList.map(_.swap)
      intermediate.map(el => (el._1.Negative, 
        (el._2._1.boundary(el._2._2).flip,
         el._2._1.boundary(mod(el._2._2 - 1, el._2._1.boundary.length))))).toMap
    }

    val vertexList = twoComplex.vertices.toList
    val edgeList = twoComplex.edges.toList

    def faceOfEdge (edge : Edge) : Polygon = {
       val face = twoComplex.faces.find(_.boundary.contains(edge))
       assert(face != None, "For a closed surface each edge should be in at least one face")
       face.get
    }
    
    // creates the face corresponding the edge. Also gives an edgepath homotopic to the edge preserving endpoints
    def getFace (edge : Edge) : (Polygon, (Edge, EdgePath))= {
      val face = faceOfEdge(edge)
      val flipFace = faceOfEdge(edge.flip)
      val indexOfEdge = face.boundary.indexOf(edge)
      val indexOfFlip = flipFace.boundary.indexOf(edge.flip)
      val periOfFace = face.boundary.length
      val periOfFlip = flipFace.boundary.length
      val edgePath = // from edge.initial to barycenter of face of edge to edge.terminal
        Append(Append(Constant(edge.initial),
          (newEdgeMap1(face, indexOfEdge).Negative)),
          (newEdgeMap1(face, mod(indexOfEdge + 1, periOfFace)).Positive))    

      val newFace = QuadFace(face, flipFace, indexOfEdge, indexOfFlip)
      (newFace, (edge, edgePath))
    }  

    val newFacesAndEdgePathMaps = twoComplex.halfEdges.map(getFace)
    val newFaces = newFacesAndEdgePathMaps.map(el => el._1)
    val halfOfEdgePathMap = newFacesAndEdgePathMaps.map(el => el._2)
    val otherHalfOfEdgePathMap = halfOfEdgePathMap.map(el => (el._1.flip, el._2.reverse))
    val edgeToEdgePathMap = (halfOfEdgePathMap ++ otherHalfOfEdgePathMap).toMap

    val quad = PureComplex(newFaces)

    assert(isQuadrangulation(quad), s"The result of the algorithm doesn't give a quadragulation")

    def forwardEdgePathMap (edgePath : EdgePath) : EdgePath = {
      require(edgePath.inTwoComplex(twoComplex), "The given edgepath is not part of the original twoComplex")
      val newPath = edgePath match {
        case Constant(vertex) => Constant(vertex)
        case Append(init, last) => forwardEdgePathMap(init).++(edgeToEdgePathMap(last))
      }
      assert(newPath.inTwoComplex(quad), "The resulting edgepath is not part of the quadrangulation of the original complex")
      newPath
    }

    def gatherEdgesUsingTurnHelper (edge : Edge, turn : Int, accum : Vector[Edge]) : Vector[Edge] = {
      require(twoComplex.edges.contains(edge), s"The Edge $edge is not inside the TwoComplex $twoComplex")
      require(twoComplex.isClosedSurface, 
        s"gatheEdgesUsingTurnHelper might not work as the TwoComplex $twoComplex is not a closed surface")
      if (turn == 0) accum
      else if (turn > 0) gatherEdgesUsingTurnHelper(twoComplex.succOpt(edge).get, turn - 1, accum :+ edge)
      else gatherEdgesUsingTurnHelper(twoComplex.succOpt(edge).get, turn + 1, accum :+ edge)
    }

    def gatherEdgesUsingTurn (edge : Edge, turn : Int) : Vector[Edge] = {
      if (turn == 0) Vector()
      else if (turn > 0) gatherEdgesUsingTurnHelper(reverseEdgeMap(edge)._1, turn - 1, Vector())
      else gatherEdgesUsingTurnHelper(reverseEdgeMap(edge)._2, turn - 1, Vector())
    }

    // Given an edge "first" from a pre existing vertex "u" to a barycenter "b" and an edge
    // "second" from "b" to another pre existing vertex "v" gives an EdgaPath homotopic to
    // "first + second" 
    def gatherTurnUsingPairOfEdges (first : Edge, second : Edge) : EdgePath = {
      val result = EdgePath.apply(gatherEdgesUsingTurn(first, quad.turnIndex(first, second)))
      assert(result.initial == first.initial, 
        s"The initial vertex ${result.initial} of the result $result of gatheTurnUsingPairOfEdges is not same as the intial vertex ${first.initial} of the first edge")
      assert(result.terminal == second.terminal, 
        s"The initial vertex ${result.terminal} of the result $result of gatheTurnUsingPairOfEdges($first, $second) is not same as the terminal vertex ${second.terminal} of the first edge $second .")
      assert(result.inTwoComplex(twoComplex), 
        s"The result $result of gatheTurnUsingPairOfEdges($first, $second) is not inside the original TwoComplex $twoComplex .")      
      result
    }

    def backWardMapHelper (edgePath : EdgePath) : EdgePath = {
      require(edgePath.inTwoComplex(twoComplex), s"The given edgepath $edgePath is not part of the twoComplex $quad")
      require(twoComplex.vertices.contains(edgePath.initial), s"The given edgepath $edgePath does not start at a pre exsting vertex")
      require(twoComplex.vertices.contains(edgePath.terminal), s"The given edgepath $edgePath does not end at a pre exsting vertex")
      require(EdgePath.length(edgePath) % 2 == 0, s"backWardMapHelper does not work for odd length paths like $edgePath")
    
      val newPath = edgePath match {
        case Constant(vertex) => Constant(vertex)
        case Append(Constant(vertex), first) => ??? // This should not happen given the fourth requirement
        case Append(Append(init, first), second) => backWardMapHelper(init).++(gatherTurnUsingPairOfEdges(first, second))
      }
      newPath
    }

    // Helper method so that we can apply backWardMapHelper after applying this method
    def prepareEdgePath (edgePath : EdgePath) : EdgePath = {

      if (edgePath.isLoop) {
        if (twoComplex.vertices.contains(edgePath.initial)) edgePath
        else {
          assert(EdgePath.length(edgePath) >= 1, s"The EdgePath $edgePath is the constant loop" ++  
            "on the barycenter $edgePath.initial. Hence it is not possible to make it loop on a pre existing vertex") 
          edgePath.shiftBasePoint
        }
      }  
      else {
        assert(twoComplex.vertices.contains(edgePath.terminal), 
         s"The EdgePath $edgePath does not end at a pre existing vertex, so backWardEdgePathMap is not valid")
        assert(twoComplex.vertices.contains(edgePath.initial), 
         s"The EdgePath $edgePath does not start at a pre existing vertex, so backWardEdgePathMap is not valid") 
        edgePath 
      }
    }

    def backWardEdgePathMap (edgePath : EdgePath) : EdgePath = {
      require(edgePath.inTwoComplex(quad), s"The given edgepath $edgePath is not part of the TwoComplex $quad")
      val newPath : EdgePath = backWardMapHelper(prepareEdgePath(edgePath))
      assert(newPath.inTwoComplex(twoComplex), s"The resulting edgepath $newPath is not part of the original TwoComplex $twoComplex")
      newPath
    }
    (quad, (forwardEdgePathMap, backWardEdgePathMap))
  } 
}