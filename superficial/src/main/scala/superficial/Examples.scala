package superficial

import EdgePair._
import Polygon.Index
import scala.collection.immutable.Nil
import superficial.Generator.a

object Triangles {
  import SphereComplex._

  object HollowTriangle extends TwoComplex {
    val sides = 3
    val faces = Set.empty
    val boundary = Vector(A.Positive, B.Positive, C.Positive)
    val vertices = Set(X, Y, Z)
    val edges = Set(A.Positive, B.Positive, C.Positive)
  }

}

object Examples {

  val pinchedTorus = 
    TwoComplex.symbolic("V1")(
      "E1" -> ("V1", "V1"), "E2" -> ("V1", "V1")
      )(
        "F1" -> Seq("E1" -> true, "E2" -> false), "F2" -> Seq("E2" -> true, "E1" -> false)
      )

  val disjointLoops =
    TwoComplex.symbolic("x", "y")("a" -> ("x", "x"), "b" -> ("y", "y"))()

  val wedgeTori = TwoComplex.symbolic("x")(
    "a1" -> ("x", "x"),
    "b1" -> ("x", "x"),
    "a2" -> ("x", "x"),
    "b2" -> ("x", "x")
  )(
    "face1" -> Seq("a1" -> true, "b1" -> true, "a1" -> false, "b1" -> false),
    "face2" -> Seq("a2" -> true, "b2" -> true, "a2" -> false, "b2" -> false)
  )
  val disk =  TwoComplex.symbolic("V")(
    "a1" -> ("V", "V")
  )(
    "face1" -> Seq("a1" -> true)
  )
            
  val torusWithBoundary = TwoComplex.symbolic("v")(
    "a1" -> ("v", "v"), "a2" -> ("v", "v"), "ab" -> ("v", "v") 
    )(
      "F" -> Seq("a1" -> true, "ab" -> true, "a2" -> true, "a1" -> false, "a2" -> false)
  )
  
}

/*Sphere with n and Surface with given genus and boundary components.*/
trait sphereWithHoles {
  def generateComplex (numberOfHoles: Int): TwoComplex
}

object sphereWithHoles {
  def generateComplex (numberOfHoles: Int) :  TwoComplex = {

    require(numberOfHoles>=1)
    var boundaryVertices = (0 to (2*numberOfHoles-1)).toArray.map(_.toString).map("v" + _)

    var nonSharedEdgesA = (0 to (numberOfHoles - 1)).toArray.map(_.toString).map("f1" + _)
    var nonSharedEdgesB = (0 to (numberOfHoles - 1)).toArray.map(_.toString).map("f2" + _)

    var RelationsA: Array[(String, (String, String))] = new Array[(String, (String, String))](numberOfHoles)
    var RelationsB: Array[(String, (String, String))] = new Array[(String, (String, String))](numberOfHoles)
    for (i <- (0 to (numberOfHoles - 1))) {
      RelationsA(i) = (nonSharedEdgesA(i),(boundaryVertices((2*i+1)%(2*numberOfHoles)), boundaryVertices((2*i+2)%(2*numberOfHoles))))
      RelationsB(i) = (nonSharedEdgesB(i),(boundaryVertices((2*i+2)%(2*numberOfHoles)), boundaryVertices((2*i+1)%(2*numberOfHoles))))
    }

    var SharedEdges = (0 to (numberOfHoles-1)).toArray.map(_.toString).map("e"+_)
    var RelationsE: Array[(String, (String, String))] = new Array[(String, (String, String))](numberOfHoles)
    for (i <- (0 to (numberOfHoles - 1))) {
      RelationsE(i) = (SharedEdges(i),(boundaryVertices((2*i)%(2*numberOfHoles)),boundaryVertices((2*i+1)%(2*numberOfHoles))))
    }

    var TempSeqU1: Array[(String, Boolean)] = new Array[(String, Boolean)](numberOfHoles)
    var TempSeqU2: Array[(String, Boolean)] = new Array[(String, Boolean)](numberOfHoles)
    var TempSeqD1: Array[(String, Boolean)] = new Array[(String, Boolean)](numberOfHoles)
    var TempSeqD2: Array[(String, Boolean)] = new Array[(String, Boolean)](numberOfHoles)

    for (i <- (0 to (numberOfHoles - 1))) {
      TempSeqU1(i) = (SharedEdges(i), true)
      TempSeqU2(i) = (nonSharedEdgesA(i), true)
      TempSeqD1(i) = (SharedEdges(i), false)
      TempSeqD2(i) = (nonSharedEdgesB(i), true)
    }

    val SeqUp = TempSeqU1.map(List(_)).zipAll(TempSeqU2.map(List(_)), Nil, Nil).flatMap(Function.tupled(_ ::: _))
    val SeqDown = TempSeqD1.map(List(_)).zipAll(TempSeqD2.map(List(_)), Nil, Nil).reverse.flatMap(Function.tupled(_:::_))

    val EdgeRelations = (RelationsA++RelationsB++RelationsE).toList

    TwoComplex.symbolic(boundaryVertices.toIndexedSeq:_*)(EdgeRelations.toIndexedSeq:_*)(("p1",SeqUp.toIndexedSeq), ("p2",SeqDown.toIndexedSeq))
  }
}


trait surfaceWithBoundary{
  def generateComplex (genus: Int, boundaryComponents: Int): TwoComplex
}


object surfaceWithBoundary{
  def generateComplex (genus: Int, boundaryComponents: Int) : TwoComplex = {

    val VertexList = (0 to boundaryComponents).toVector.map(_.toString).map("v"+_)
    val normalEdges = (3 to 2*genus).toArray.map(_.toString).map("e"+_)

    var usualRelations: Array[(String, (String, String))] = new Array[(String, (String, String))](2*genus-2)
    for (i <- (0 to (2*genus-3))) {
      usualRelations(i) = (normalEdges(i),(VertexList(0), VertexList(0)))
    }

    var boundaries = (1 to boundaryComponents).toVector.map(_.toString).map("b"+_)
    var boundaryRelations: Array[(String, (String, String))] = new Array[(String, (String, String))](boundaryComponents)
    for (i <- (0 to (boundaryComponents-1))) {
      boundaryRelations(i) = (boundaries(i),(VertexList(i+1), VertexList(i+1)))
    }

    val boundaryB = Vector(("e2",(VertexList(0), VertexList(0))))

    val intersectionEdges = (0 to boundaryComponents).toVector.map(_.toString).map("a"+_)
    var intersectionRelations: Array[(String, (String, String))] = new Array[(String, (String, String))](boundaryComponents+1)
    for (i <- (0 to (boundaryComponents))) {
      intersectionRelations(i) = (intersectionEdges(i),(VertexList(i%(boundaryComponents+1)), VertexList((i+1)%(boundaryComponents+1))))
    }

    val SumRelations = (usualRelations++boundaryRelations++intersectionRelations++boundaryB)

    var TempUp: Array[(String, Boolean)] = new Array[(String, Boolean)](boundaryComponents+1)
    var TempUpR: Array[(String, Boolean)] = new Array[(String, Boolean)](boundaryComponents+1)
    var TempBound: Array[(String, Boolean)] = new Array[(String, Boolean)](boundaryComponents)

    for (i <- (0 to (boundaryComponents))) {
      TempUp(i) = (intersectionEdges(i), true)
      TempUpR(i) = (intersectionEdges(i), false)
    }
    for (i <- (0 to boundaryComponents-1)) {
      TempBound(i) = (boundaries(i), true)
    }

    val firstn = TempUp.map(List(_)).zipAll(TempBound.map(List(_)), Nil, Nil).flatMap(Function.tupled(_ ::: _))
    val secondn = List(("e2", true))
    val thirdn = TempUpR.toList.reverse
    val fourthn = List(("e2", false))

    val initHas = (firstn++secondn++thirdn++fourthn).toSeq

    var duals1: Array[(String, Boolean)] = new Array[(String, Boolean)](2*genus-2)
    var duals2: Array[(String, Boolean)] = new Array[(String, Boolean)](2*genus-2)

    for (i <- (0 to (genus-2))) {
      duals1(2*i) = (normalEdges(2*i), true)
      duals1(2*i+1) = (normalEdges(2*i), false)
      duals2(2*i) = (normalEdges(2*i+1), true)
      duals2(2*i+1) = (normalEdges(2*i+1), false)
    }

    val duals = duals1.map(List(_)).zipAll(duals2.map(List(_)), Nil, Nil).flatMap(Function.tupled(_ ::: _)).toSeq
    val boundarySeq = initHas++duals

    TwoComplex.symbolic(VertexList.toIndexedSeq:_*)(SumRelations.toIndexedSeq:_*)("P" -> boundarySeq)
    
  }
}