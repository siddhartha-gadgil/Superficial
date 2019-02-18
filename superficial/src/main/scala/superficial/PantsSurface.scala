package superficial

import PantsSurface._, Polygon.Index
import scala.math._

case class Z3(n: Int) {
  require(0 <= n && n <= 2, s"$n not valid mod 3 representative")

  def next = Z3((n + 1) % 3)

  def prev = Z3((n + 2) % 3)

  def <(that: Z3): Boolean = n < that.n

  def others: Set[Z3] = Z3.enum.toSet - this
}

object Z3 {
  val enum: Vector[Z3] = Vector(0, 1, 2).map(Z3(_))
}

/**
  * index for boundary of pants, may be in the curve system
  * or the boundary of the surface
  *
  * @param pants     the index of the pair of pants
  * @param direction the prong of the pair of pants
  */
case class PantsBoundary(pants: Index, direction: Z3) {
  def prev = PantsBoundary(pants - 1, direction)

  def dropOpt(n: Index): Option[PantsBoundary] =
    if (pants == n) None
    else if (pants > n) Some(prev)
    else Some(this)

  def <(that: PantsBoundary): Boolean =
    (pants < that.pants) || ((pants == that.pants) && (direction < that.direction))
}

case class BoundaryVertex(pb: PantsBoundary, first: Boolean) extends Vertex

case class BoundaryEdge(pb: PantsBoundary,
                        top: Boolean,
                        positiveOriented: Boolean)
    extends Edge {
  lazy val flip = BoundaryEdge(pb, top, !positiveOriented)
  lazy val terminal: BoundaryVertex =
    BoundaryVertex(pb, positiveOriented)

  lazy val initial: BoundaryVertex =
    BoundaryVertex(pb, !positiveOriented)

}

abstract class Hexagon extends Polygon{
  val sides = 6
}

object Hexagon{
  def arccosh(x: Double): Double = log(x + sqrt(x * x - 1))

  def arcsinh(x: Double): Double = log(x + sqrt(x * x + 1))

  def side(l1: Double, l2: Double, l3: Double): Double =
    arccosh((cosh(l1) * cosh(l2)  + cosh(l3))/ (sinh(l1) * sinh(l2)))

  def length2(a: Double, b: Double): Double = arccosh(cosh(a) * cosh(b))

  def length3(a: Double, l: Double, b: Double): Double =
    arccosh(cosh(a) * cosh(l) * cosh(b) - sinh(a) * sinh(b))

  def length4(a: Double, l1: Double, l2: Double, b: Double): Double =
    arccosh(cosh(a) * cosh(l1) * cosh(l2) * cosh(b) -
      sinh(a) * sinh(l2) * cosh(b) - cosh(a) * sinh(l1) * sinh(b))

  def mod6(n: Int): Index = {
    val m = n % 6
    if (m >= 0) m else m + 6
  } ensuring((m) => 0 <= m && m < 6)

  case class Hyperbolic(a: Double, b: Double, c: Double) {
    def sideLength(n: Index) : Double =
      if (n % 2 == 0) Vector(a, b, c)(n/2)
      else side(length(n - 1), length(n + 1), length(n+3))

    def length(n: Int): Double = sideLength(mod6(n))

    def getArcLength(i: Int, j: Int, initShift: Double, lastShift: Double): Double = {
      assert( i < j && j < i + 4, s"getArcLength called with $i, $j" )
      val init = length(i) - initShift
      val last = lastShift
      j - i match {
        case 1 => length2(init, last)
        case 2 => length3(init, length(i + 1) ,last)
        case 3 => length4(init, length(i + 1), length(i + 2), last)
      }
    }

    def arcLength(i: Index, j: Index, initShift: Double, lastShift: Double): Double =
      if (i > j) arcLength(j, i, lastShift, initShift)
      else if (j - i <4) getArcLength(i, j, initShift, lastShift)
      else getArcLength(j, i + 6, lastShift, initShift)
  }
}

case class PantsSeam(pants: Index, initial: Vertex, terminal: Vertex)
    extends Edge {
  lazy val flip = PantsSeam(pants, terminal, initial)
}

case class Curve(left: PantsBoundary, right: PantsBoundary) {
  val support: Set[PantsBoundary] = Set(left, right)

  val neighbours: Set[Index] = support.map(_.pants)

  def contains(pb: PantsBoundary): Boolean = support.contains(pb)

  def dropOpt(n: Index): Option[Curve] =
    for {
      newLeft <- left.dropOpt(n)
      newRight <- right.dropOpt(n)
    } yield Curve(newLeft, newRight)
}

import SkewCurve._

case class SkewCurve(left: PantsBoundary, right: PantsBoundary, twist: Double) { curve =>
  val skewLess : Boolean = twist == 0 || twist == 0.5

  val support: Set[PantsBoundary] = Set(left, right)

  val neighbours: Set[Index] = support.map(_.pants)

  val shift = math.min(twist, mod1(twist + 0.5))

  def contains(pb: PantsBoundary): Boolean = support.contains(pb)

  def nextVertex(position: Double) : Double = 
    if (skewLess){
      assert(position == 0 || position == 0.5, s"twist is $twist but vertex at $position")
      mod1(position + 0.5)
    } else 
    if (position == 0 || position == 0.5) position + shift 
    else if (position < 0.5) 0.5
    else 0

  def previousVertex(position: Double) : Double = 
  if (skewLess){
    assert(position == 0 || position == 0.5, s"twist is $twist but vertex at $position")
    mod1(position + 0.5)
  } else 
  if (position == 0) 0.5 + shift 
  else if (position == 0.5) shift
  else if (position > 0.5) 0.5
  else 0

  def shiftedVertex(position: Double, positivelyOriented : Boolean) =
    if (positivelyOriented) nextVertex(position) else (previousVertex(position))

}

object SkewCurve{
  def mod1(x: Double) = x - math.floor(x)
}



case class CurveVertex(curve: Curve, first: Boolean) extends Vertex

case class SkewCurveVertex(curve: SkewCurve, position: Double) extends Vertex

case class CurveEdge(curve: Curve, top: Boolean, positivelyOriented: Boolean)
    extends Edge {
  lazy val flip = CurveEdge(curve, top, !positivelyOriented)

  lazy val initial: Vertex =
    CurveVertex(curve, !(positivelyOriented ^ top))

  lazy val terminal: Vertex =
    CurveVertex(curve, positivelyOriented ^ top)
}

case class SkewCurveEdge(curve: SkewCurve, position: Double, positivelyOriented: Boolean) extends Edge{
  lazy val finalPosition = if (positivelyOriented) curve.nextVertex(position) else curve.previousVertex(position)
  lazy val flip = SkewCurveEdge(curve, finalPosition, !positivelyOriented)

  lazy val initial = SkewCurveVertex(curve, position)

  lazy val terminal = SkewCurveVertex(curve, finalPosition)

}

case class PantsHexagon(pants: Index, top: Boolean, cs: Set[Curve])
    extends Hexagon {
  val vertices: Set[Vertex] =
    for {
      direction: Z3 <- Z3.enum.toSet
      first <- Set(true, false)
    } yield vertex(PantsBoundary(pants, direction), first, cs)

  val boundary: Vector[Edge] =
    for {
      direction <- Z3.enum
      e <- Vector(edge(PantsBoundary(pants, direction),
                       top,
                       positivelyOriented = top,
                       cs),
                  seam(pants, direction, cs))
    } yield e

}

case class PantsSurface(numPants: Index, cs: Set[Curve])
    extends PureTwoComplex {
  val indices: Vector[Index] = (0 until numPants).toVector
  
  val faces: Set[Polygon] =
    for {
      pants: Index <- indices.toSet
      top <- Set(true, false)
    } yield PantsHexagon(pants, top, cs)

  val allCurves: Set[PantsBoundary] =
    for {
      direction: Z3 <- Z3.enum.toSet
      pants <- indices
    } yield PantsBoundary(pants, direction)

  val csSupp: Set[PantsBoundary] = cs.flatMap(_.support)

  val boundaryCurves: Set[PantsBoundary] = allCurves -- csSupp

  val loopIndices: Set[Index] =
    cs.collect {
      case p if p.left.pants == p.right.pants => p.left.pants
    }

  val boundaryIndices: Set[Index] = boundaryCurves.map(_.pants)

  def isClosed: Boolean = boundaryIndices.isEmpty

  def innerCurves(index: Index): Int =
    csSupp.count((p) => p.pants == index)

  def drop(n: Index): PantsSurface =
    PantsSurface(numPants - 1, cs.flatMap(_.dropOpt(n)))

  def neighbourhood(pantSet: Set[Index]): Set[Index] =
    indices
      .filter(
        (m) =>
          cs.exists((curve) =>
            curve.neighbours
              .contains(m) && curve.neighbours.intersect(pantSet).nonEmpty))
      .toSet

  @annotation.tailrec
  final def component(pantSet: Set[Index]): Set[Index] = {
    val expand = neighbourhood(pantSet)
    if (expand == pantSet) expand
    else component(expand)
  }

  lazy val isConnected: Boolean =
    (numPants <= 1) || (component(Set(0)) == indices.toSet)

  lazy val peripheral: Set[Index] =
    indices.filter((m) => drop(m).isConnected).toSet

//  assert(numPants ==0 || (loopIndices union boundaryIndices union peripheral).nonEmpty, s"strange $this")

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

  def allGlue2: Set[PantsSurface] =
    for {
      pb1 <- boundaryCurves
      pb2 <- boundaryCurves
      if pb2 < pb1
    } yield glue2(pb1, pb2)

  def allGlue3: Set[PantsSurface] =
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

  def bers(g: Int)= 26 * (g - 1)

  val margulis = Hexagon.arccosh(sqrt((2 * cos(2 * Pi/7) - 1)/(8 * cos(Pi/7) + 7)))

  def isomorphic(first: PantsSurface, second: PantsSurface): Boolean =
    if (first.numPants == 0) second.numPants == 0
    else
      (first == second) || { // quick checks first
        first.boundaryIndices.size == second.boundaryIndices.size &&
        first.loopIndices.size == second.loopIndices.size && {
          if (first.loopIndices.nonEmpty) {
            val pruned = first.drop(first.loopIndices.head)
            val loops = second.loopIndices
            val secondPruned = loops.map((n) => second.drop(n))
            secondPruned.exists((surf) => isomorphic(pruned, surf))
          } else if (first.boundaryIndices.nonEmpty) {
            val ind = first.boundaryIndices.head
            val pruned = first.drop(ind)
            val secondIndices = second.boundaryIndices.filter((n) =>
              second.innerCurves(n) == first.innerCurves(ind))
            val secondPruned = secondIndices.map((n) => second.drop(n))
            secondPruned.exists((surf) => isomorphic(pruned, surf))
          } else { // peripheral ones must have no loops or boundaries
            val ind = first.peripheral.head
            val pruned = first.drop(ind)
            val secondIndices = second.peripheral
            val secondPruned = secondIndices.map((n) => second.drop(n))
            (first.peripheral.size == second.peripheral.size) && secondPruned
              .exists((surf) => isomorphic(pruned, surf))
          }
        }
      }

  def distinct(surfaces: Vector[PantsSurface]): Vector[PantsSurface] =
    surfaces match {
      case Vector()         => Vector()
      case head +: Vector() => Vector(head)
      case head +: tail =>
        val newTail = distinct(tail)
        if (newTail.exists(isomorphic(_, head))) newTail
        else head +: newTail
    }

  val all: Stream[Vector[PantsSurface]] = Stream.from(0).map(getAll)

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

  def getCurve(pb: PantsBoundary, cs: Set[Curve]): Option[(Curve, Boolean)] =
    cs.find(
      (c) => c.left == pb
    ).map((c) => c -> true).orElse(
      cs.find(
        (c) => c.right == pb
      ).map((c) => c -> false)
    )

  def edge(pb: PantsBoundary,
           top: Boolean,
           positivelyOriented: Boolean,
           cs: Set[Curve]): Edge =
    getCurve(pb, cs)
      .map {
        case (curve, positive) => CurveEdge(curve, top, positive)
      }
      .getOrElse(BoundaryEdge(pb, top, positivelyOriented))

  def vertex(pb: PantsBoundary, first: Boolean, cs: Set[Curve]): Vertex =
    getCurve(pb, cs)
      .map {
        case (curve, positive) => CurveVertex(curve, !(first ^ positive))
      }
      .getOrElse(BoundaryVertex(pb, first))

  def seam(pants: Index, direction: Z3, cs: Set[Curve]): PantsSeam = {
    val initial = vertex(PantsBoundary(pants, direction), first = false, cs)
    val terminal = vertex(PantsBoundary(pants, direction.next), first = true, cs)
    PantsSeam(pants, initial, terminal)
  }
}
