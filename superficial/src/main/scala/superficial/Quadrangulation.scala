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

  /**
   *Gives the quadrangulation of a twocomplex along maps from edgepaths from the twocomplex to  
   *paths in the quadragulation and from edgepaths from the quadragulation to the edgepaths in the
   * twocomplex. 
  */

  def quadrangulate (twoComplex : TwoComplex) : (TwoComplex, (EdgePath => EdgePath, EdgePath => EdgePath)) = {
 
    require(twoComplex.isClosedSurface, "Algorithm only works for closed surfaces")       

    val faceList = twoComplex.faces.toList
    val facesWithIndexes :  List[(Polygon, Int)] = faceList.flatMap(f => ((0 to (f.boundary.length - 1)).map(ind => (f, ind))))

    def createBarycenter (face : Polygon) : Vertex = {
      object bFace extends Vertex
      bFace
    } 

    val barycentersList = faceList.map(createBarycenter(_))
    val barycenters = faceList.zip(barycentersList).toMap

    def createEdgePairs (face : Polygon, index : Int) : EdgePair = {
      new EdgePair(barycenters(face), face.boundary(index).terminal)
    }

    val newEdgeMap1 : Map[(Polygon, Int),EdgePair] = 
      facesWithIndexes.map{
        case (poly, j) => (poly, j) -> createEdgePairs(poly, j)
      }.toMap

    def mod (m : Int, n : Int) = ((m % n) + n) % n 
  
    val vertexList = twoComplex.vertices.toList
    val edgeList = twoComplex.edges.toList

    def faceOfEdge (edge : Edge) : Polygon = {
       val face = twoComplex.faces.find(_.boundary.contains(edge))
       assert(face != None, "For a closed surface each edge should be in at least one face")
       face.get
    }
    
    // creates the face corresponding the edge. Also gives an edgepath homotopic to the edge preserving endpoints
    def createFace (edge : Edge) : (Polygon, ((Edge, EdgePath), Vector[(EdgePath, Edge)]))= {
      val face = faceOfEdge(edge)
      val flipFace = faceOfEdge(edge.flip)
      val indexOfEdge = face.boundary.indexOf(edge)
      val indexOfFlip = flipFace.boundary.indexOf(edge.flip)
      val periOfFace = face.boundary.length
      val periOfFlip = flipFace.boundary.length
      val edgePath = // from edge.intial to barycenter of face of edge to edge.terminal
        Append(Append(Constant(edge.initial),
          (newEdgeMap1(face, mod(indexOfEdge - 1, periOfFace)).Negative)),
          (newEdgeMap1(face, indexOfEdge).Positive)) 

      val edgePathThroughFlipFace = // from edge.intial to barycenter of face of edge.flip to edge.terminal
        Append(Append(Constant(edge.initial),
          (newEdgeMap1(flipFace, indexOfFlip).Negative)),
          (newEdgeMap1(flipFace, mod(indexOfFlip - 1, periOfFlip)).Positive))

      val backWardMapAsVector = Vector(
        (edgePath, edge),
        (edgePathThroughFlipFace, edge),
        (edgePath.reverse, edge.flip),
        (edgePathThroughFlipFace.reverse, edge.flip) 
      )        

      val newFace = Polygon.apply(Vector(
        newEdgeMap1(face, indexOfEdge).Positive, // from barycenter of face of edge to edge.terminal
        newEdgeMap1(flipFace, mod(indexOfFlip - 1, periOfFlip)).Negative, // from edge.terminal to barycenter of face of edge.flip
        newEdgeMap1(flipFace, indexOfFlip).Positive, // from barycenter of face of edge.flip to edge.intial
        newEdgeMap1(face, mod(indexOfEdge - 1, periOfFace)).Negative // from edge.intial to barycenter of face of edge
      ))

      (newFace, ((edge, edgePath), backWardMapAsVector))
    }  

    val newFacesAndEdgePathMaps = twoComplex.halfEdges.map(createFace)
    val newFaces = newFacesAndEdgePathMaps.map(el => el._1)
    val halfOfEdgePathMap = newFacesAndEdgePathMaps.map(el => (el._2)._1)
    val otherHalfOfEdgePathMap = halfOfEdgePathMap.map(el => (el._1.flip, el._2.reverse))
    val edgeToEdgePathMap = (halfOfEdgePathMap ++ otherHalfOfEdgePathMap).toMap
    
    // This maps paths of length two which are not spurs to corresponding edges.
    // These paths need to be from a pre-existing vertex to another pre-existing vertex, i.e 
    // vertices which are not barycenters
    val edgePathToEdgeMap = newFacesAndEdgePathMaps.flatMap(el => (el._2)._2).toMap

    def forwardEdgePathMap (edgePath : EdgePath) : EdgePath = {
      require(edgePath.inTwoComplex(twoComplex), "The given edgepath is not part of the original twoComplex")
      val newPath = edgePath match {
        case Constant(vertex) => Constant(vertex)
        case Append(init, last) => forwardEdgePathMap(init).++(edgeToEdgePathMap(last))
      }
      assert(newPath.inTwoComplex(quad), "The resulting edgepath is not part of the quadrangulation of the original complex")
      newPath
    }

    // This is edgePath map from the quadrangulation to the original twoComplex.
    // It replies on the assumption that the edgePath given as input is reduced and
    // both starts and ends on vertices which are not barycenters
    def backWardEdgePathMapHelper (edgePath : EdgePath) : EdgePath = {
      require(twoComplex.vertices.contains(edgePath.initial), 
        s"EdgePath $edgePath starts at ${edgePath.initial} which is not a pre-existing vertex.")

      require(twoComplex.vertices.contains(edgePath.terminal), 
        s"EdgePath $edgePath ends at ${edgePath.terminal} which is not a pre-existing vertex.") 
      
      // require(EdgePath.isReduced(edgePath), s"$edgePath is not reduced")

      edgePath match {
        case Constant(initial) => Constant(initial)
        case Append(Append(init, e1), e2) => {
          backWardEdgePathMapHelper(init) ++ 
          EdgePath.apply(Vector(edgePathToEdgeMap(EdgePath.apply(Vector(e1, e2)))))
        }
        case Append(Constant(initial), e) => ???
      }                  
    }

    object quad extends PureTwoComplex {
      val faces = newFaces
    }

    assert(isQuadrangulation(quad), s"The result of the algorithm doesn't give a quadragulation")
    (quad, (forwardEdgePathMap, ???))
  } 
}