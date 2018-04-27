package superficial

import PantsSurface._

case class Z3(n: Int) {
  require(0 <= n && n <= 2, s"$n not valid mod 3 representative")
  def next = Z3((n + 1) % 3)
  def prev = Z3((n + 2) % 3)

  def <(that: Z3) = n < that.n

  def others: Set[Z3] = Z3.enum - this
}

object Z3 {
  val enum: Set[Z3] = Set(0, 1, 2).map(Z3(_))
}

/**
  * index for boundary of pants, may be in the curve system
  * or the boundary of the surface
  * @param pants the index of the pair of pants
  * @param direction the prong of the pair of pants
  */
case class PantsBoundary(pants: Index, direction: Z3) {
  def prev = PantsBoundary(pants - 1, direction)

  def dropOpt(n: Index): Option[PantsBoundary] =
    if (pants ==n) None
    else if (pants > n) Some(prev) else Some(this)

  def <(that: PantsBoundary) =
    (pants < that.pants) || ((pants == that.pants) && (direction < that.direction))
}

case class BoundaryVertex(pb: PantsBoundary, first: Boolean) extends Vertex

case class BoundaryEdge(pb: PantsBoundary,
                        top: Boolean,
                        positiveOriented: Boolean)
    extends Edge {
  lazy val flip = BoundaryEdge(pb, top, !positiveOriented)
  lazy val head: BoundaryVertex =
    BoundaryVertex(pb, positiveOriented)

  lazy val tail: BoundaryVertex =
    BoundaryVertex(pb, !positiveOriented)

}

abstract class Hexagon extends Polygon(6)

case class PantsSeam(pants: Index, head: Vertex, tail: Vertex) extends Edge {
  lazy val flip = PantsSeam(pants, tail, head)
}

case class Curve(left: PantsBoundary, right: PantsBoundary) {
  val support: Set[PantsBoundary] = Set(left, right)

  def contains(pb: PantsBoundary): Boolean = support.contains(pb)

  def dropOpt(n: Index) =
    for {
      newLeft <- left.dropOpt(n)
      newRight <- right.dropOpt(n)
    } yield Curve(newLeft, newRight)
}

case class CurveVertex(curve: Curve, first: Boolean) extends Vertex

case class CurveEdge(curve: Curve, top: Boolean, positivelyOriented: Boolean)
    extends Edge {
  lazy val flip = CurveEdge(curve, top, !positivelyOriented)

  lazy val head: Vertex =
    CurveVertex(curve, positivelyOriented ^ top)

  lazy val tail: Vertex =
    CurveVertex(curve, !(positivelyOriented ^ top))
}

case class PantsHexagon(pants: Index, top: Boolean, cs: Set[Curve])
    extends Hexagon {
  val vertices: Set[Vertex] =
    for {
      direction <- Z3.enum
      first <- Set(true, false)
    } yield vertex(PantsBoundary(pants, direction), first, cs)

  val seams: Set[Edge] =
    for {
      direction <- Z3.enum
    } yield seam(pants, direction, cs)

  val boundaryEdges: Set[Edge] =
    for {
      direction <- Z3.enum
      top <- Set(true, false)
      positivelyOriented <- Set(true, false)
    } yield edge(PantsBoundary(pants, direction), top, positivelyOriented, cs)

  val edges: Set[Edge] = seams union boundaryEdges
}

case class PantsSurface(numPants: Index, cs: Set[Curve])
    extends PureTwoComplex {
  val faces: Set[Polygon] =
    for {
      pants: Index <- (0 until numPants).toSet
      top <- Set(true, false)
    } yield PantsHexagon(pants, top, cs)

  val allCurves: Set[PantsBoundary] =
    for {
      direction <- Z3.enum
      pants <- 0 until numPants
    } yield PantsBoundary(pants, direction)

  val csSupp: Set[PantsBoundary] = cs.flatMap(_.support)

  val boundaryCurves: Set[PantsBoundary] = allCurves -- csSupp

  val loopIndices: Set[Index] =
    cs.collect {
      case p if p.left.pants == p.right.pants => p.left.pants
    }

  val boundaryIndices: Set[Index] = boundaryCurves.map(_.pants)

  def isClosed = boundaryIndices.isEmpty

  def innerCurves(index: Index): Int =
    csSupp.count((p) => p.pants == index)

  def drop(n: Index): PantsSurface =
    PantsSurface(numPants - 1, cs.flatMap(_.dropOpt(n)))

  def glue1(pb: PantsBoundary) =
    PantsSurface(numPants + 1, cs + Curve(pb, PantsBoundary(numPants, Z3(0))))

  def glue2(pb1: PantsBoundary, pb2: PantsBoundary) =
    PantsSurface(
      numPants + 1,
      cs union Set(
        Curve(pb1, PantsBoundary(numPants, Z3(0))),
        Curve(pb2, PantsBoundary(numPants, Z3(1)))
      )
    )

  def glue3(pb1: PantsBoundary, pb2: PantsBoundary, pb3: PantsBoundary) =
    PantsSurface(
      numPants + 1,
      cs union Set(
        Curve(pb1, PantsBoundary(numPants, Z3(0))),
        Curve(pb2, PantsBoundary(numPants, Z3(1))),
        Curve(pb3, PantsBoundary(numPants, Z3(2)))
      )
    )

  def glueLoop(pb: PantsBoundary) =
    PantsSurface(
      numPants + 1,
      cs union Set(
        Curve(pb, PantsBoundary(numPants, Z3(0))),
        Curve(PantsBoundary(numPants, Z3(1)), PantsBoundary(numPants, Z3(2)))
      )
    )

  def allGlue1: Set[PantsSurface] = boundaryCurves.map(glue1)

  def allGlueLoop: Set[PantsSurface] = boundaryCurves.map(glueLoop)

  def allGlue2 =
    for {
      pb1 <- boundaryCurves
      pb2 <- boundaryCurves
      if pb2 < pb1
    } yield glue2(pb1, pb2)

  def allGlue3 =
    for {
      pb1 <- boundaryCurves
      pb2 <- boundaryCurves
      if pb1 < pb2
      pb3 <- boundaryCurves
      if pb2 < pb3
    } yield glue3(pb1, pb2, pb3)

  def allGlued: Set[PantsSurface] =
    allGlue1 union allGlue2 union allGlue3 union allGlueLoop

}

object PantsSurface {
  type Index = Int

  def isomorphic(first: PantsSurface, second: PantsSurface): Boolean = {
    if (first.numPants == 0) second.numPants == 0
    else if (first == second) true
    else if (first.loopIndices.nonEmpty) {
      val pruned = first.drop(first.loopIndices.head)
      val loops = second.loopIndices
      val secondPruned = loops.map((n) => second.drop(n))
      secondPruned.exists((surf) => isomorphic(pruned, surf))
    } else if (second.loopIndices.nonEmpty) false
    else if (first.boundaryIndices.isEmpty) false
    else {
      val ind = first.boundaryIndices.head
      val pruned = first.drop(ind)
      val secondIndices = second.boundaryIndices.filter((n) =>
        second.innerCurves(n) == first.innerCurves(ind))
      val secondPruned = secondIndices.map((n) => second.drop(n))
      secondPruned.exists((surf) => isomorphic(pruned, surf))
    }
  }

  def distinct(surfaces: Vector[PantsSurface]): Vector[PantsSurface] =
    surfaces match {
      case Vector() => Vector()
      case head +: Vector() => Vector(head)
      case head +: tail =>
        val newTail = distinct(tail)
//        println(newTail.size)
        if (newTail.exists(isomorphic(_, head))) newTail
        else head +: newTail
    }

  val all = Stream.from(0).map(getAll)

  def getAll(n: Int): Vector[PantsSurface] =
    if (n == 0) Vector()
    else if (n == 1)
      Vector(
        PantsSurface(1, Set()),
        PantsSurface(
          1,
          Set(Curve(PantsBoundary(0, Z3(0)), PantsBoundary(0, Z3(1)))))
      )
    else
      distinct(
        getAll(n - 1).flatMap(
          (s) => s.allGlued.toVector
        ))

  def allClosed: Stream[Vector[PantsSurface]] = all.map(_.filter(_.isClosed))


  def getCurve(pb: PantsBoundary, cs: Set[Curve]): Option[Curve] =
    cs.find(
      (c) => c.left == pb || c.right == pb
    )

  def edge(pb: PantsBoundary,
           top: Boolean,
           positvelyOriented: Boolean,
           cs: Set[Curve]): Edge =
    getCurve(pb, cs)
      .map(
        (curve) => CurveEdge(curve, top, positvelyOriented)
      )
      .getOrElse(BoundaryEdge(pb, top, positvelyOriented))

  def vertex(pb: PantsBoundary, first: Boolean, cs: Set[Curve]): Vertex =
    getCurve(pb, cs)
      .map(
        (curve) => CurveVertex(curve, first)
      )
      .getOrElse(BoundaryVertex(pb, first))

  def seam(pants: Index, direction: Z3, cs: Set[Curve]): PantsSeam = {
    val head = vertex(PantsBoundary(pants, direction), first = true, cs)
    val tail = vertex(PantsBoundary(pants, direction.next), first = false, cs)
    PantsSeam(pants, head, tail)
  }
}
