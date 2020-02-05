package superficial

import Polygon.Index
import scala.collection.immutable.Nil
import superficial.Generator.a
import TwoComplex._
import EdgePath._

object Quadrangulation {

  def isQuadrangulation (twoComplex : TwoComplex) : Boolean = {
    twoComplex.faces.forall(f => (f.boundary.length == 4))
  }

  /**
   *Gives the quadrangulation of a twocomplex along maps from edgepaths from the twocomplex to  
   *paths in the quadragulation. 
  */

  def quadrangulate (twoComplex : TwoComplex, path : EdgePath) : (TwoComplex, EdgePath) = {
 
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
    
    def createFace (edge : Edge) : Polygon = {
      val face = faceOfEdge(edge)
      val flipFace = faceOfEdge(edge.flip)
      val indexOfEdge = face.boundary.indexOf(edge)
      val indexOfFlip = flipFace.boundary.indexOf(edge.flip)
      val periOfFace = face.boundary.length
      val periOfFlip = flipFace.boundary.length

      Polygon.apply(Vector(
        newEdgeMap1(face, indexOfEdge).Positive,
        newEdgeMap1(flipFace, mod(indexOfFlip - 1, periOfFlip)).Negative,
        newEdgeMap1(flipFace, indexOfFlip).Positive,
        newEdgeMap1(face, mod(indexOfEdge - 1, periOfFace)).Negative
      ))
    }  

    val newFaces = twoComplex.halfEdges.map(createFace)
    object quad extends PureTwoComplex {
      val faces = newFaces
    }
    assert(isQuadrangulation(quad), s"The result of the algorithm doesn't give a quadragulation")
    //quad
    ???
  }

  /*
   * Assumption - quad is the quadragulation of twoComplex
   *
  */ 
  def forwardEdgePathMap (twoComplex : TwoComplex, quad : TwoComplex, path : EdgePath) : EdgePath = {
    
    val barycenters = quad.vertices -- twoComplex.vertices // because barycenters are the new vertices 


    ???
  }

}