package superficial

import PantsSurface._, Polygon.Index
import scala.math._
import upickle.default.{ReadWriter => RW, macroRW, _}

/**
  * One of 0, 1 and 2, indexing the boundaries of a pair of pants.
  * Class created to avoid accidentally mixing with integers
  *
  * @param n
  */
case class Z3(n: Int) extends AnyVal {
  // require(0 <= n && n <= 2, s"$n not valid mod 3 representative")

  def next = Z3((n + 1) % 3)

  def prev = Z3((n + 2) % 3)

  def <(that: Z3): Boolean = n < that.n

  def others: Set[Z3] = Z3.enum.toSet - this
}

object Z3 {

  /**
    * Enumeration of the indices
    */
  val enum: Vector[Z3] = Vector(0, 1, 2).map(Z3(_))

  /**
    * flipped enumeration
    */
  val flipEnum = Vector(0, 2, 1).map(Z3(_))

  implicit val rw: RW[Z3] = macroRW
}

/**
  * label for boundary of pants, may be a curve in the curve system
  * or a component of the boundary of the surface; if part of the curve system
  * it will be identified with another PantsBoundary
  *
  * @param pants     the index of the pair of pants
  * @param direction the prong of the pair of pants
  */
case class PantsBoundary(pants: Index, direction: Z3) {
  def prev = {
    require(pants > 0)
    PantsBoundary(pants - 1, direction)
  }

  /**
    * When pants with specified index are deleted, returns pants boundary optionally;
    * if part of the deleleted pants returns `None` else returns the pants
    *
    * @param n the index of pants that are deleted
    * @return optional pants bouundary after deletion, with index shifted if needed.
    */
  def dropOpt(n: Index): Option[PantsBoundary] =
    if (pants == n) None
    else if (pants > n) Some(prev)
    else Some(this)

  /**
    * less than for order on pants boundaries, lexicographic
    *
    * @param that
    * @return
    */
  def <(that: PantsBoundary): Boolean =
    (pants < that.pants) || ((pants == that.pants) && (direction < that.direction))
}

object PantsBoundary {
  implicit val rw: RW[PantsBoundary] = macroRW
}

/**
  * Vertex on the boundary of a pair of pants decomposed into a pair of hexagons
  *
  * @param pb the component of the boundary on which the vertex lies
  * @param isFirst whether the vertex is the first vertex.
  */
case class BoundaryVertex(pb: PantsBoundary, isFirst: Boolean) extends Vertex

/**
  * An edge in the boundary of a pair of pants which is not identified with a boundary component of another pair of pants,
  * i.e., this is part of the boundary of the surface.
  *
  * @param pb the pants boundary of which this is a part
  * @param top whether this is in the top face
  * @param positivelyOriented whether the edge is positively oriented
  */
case class BoundaryEdge(
    pb: PantsBoundary,
    top: Boolean,
    positivelyOriented: Boolean
) extends OrientedEdge {
  lazy val flip = BoundaryEdge(pb, top, !positivelyOriented)
  lazy val terminal: BoundaryVertex =
    BoundaryVertex(pb, !positivelyOriented)

  lazy val initial: BoundaryVertex =
    BoundaryVertex(pb, positivelyOriented)

}

/**
  * A hexagon
  */
trait Hexagon extends Polygon {
  val sides = 6
}

/**
  * Formulas for hyperbolic hexagons
  */
object Hexagon {
  def arccosh(x: Double): Double = log(x + sqrt(x * x - 1))

  def arcsinh(x: Double): Double = log(x + sqrt(x * x + 1))

  /**
    * length of a side of a hyperbolic right-angled hexagon given lengths of adjacent sides and the opposite side
    *
    * @param l1
    * @param l2
    * @param l3
    * @return
    */
  def side(l1: Double, l2: Double, l3: Double): Double =
    arccosh((cosh(l1) * cosh(l2) + cosh(l3)) / (sinh(l1) * sinh(l2)))

  /**
    * distance between points on adjacent sides of a right-angled hexagon
    *
    * @param a distance of first point from the common vertex
    * @param b distance of second point from the common vertex
    * @return distance between the points
    */
  def distance2(a: Double, b: Double): Double = arccosh(cosh(a) * cosh(b))

  /**
    * distance between points on sides of a right-angled hexagon with one side in between
    *
    * @param a distance of the first vertex to the (common vertex with) the side in between
    * @param l length of the first side in between.
    * @param b distance of the first vertex to the (common vertex with) the side in between
    * @return
    */
  def distance3(a: Double, l: Double, b: Double): Double =
    arccosh(cosh(a) * cosh(l) * cosh(b) - sinh(a) * sinh(b))

  def distance4(a: Double, l1: Double, l2: Double, b: Double): Double =
    arccosh(
      cosh(a) * cosh(l1) * cosh(l2) * cosh(b) -
        sinh(a) * sinh(l2) * cosh(b) - cosh(a) * sinh(l1) * sinh(b)
    )

  def mod6(n: Int): Index = {
    val m = n % 6
    if (m >= 0) m else m + 6
  } ensuring ((m) => 0 <= m && m < 6)

  /**
    * Hyperbolic right-angled hexagon determined by lengths of alternating sides
    *
    * @param a the first side
    * @param b the third side
    * @param c the fifth side
    */
  case class Hyperbolic(a: Double, b: Double, c: Double) {
    def sideLengthFun(n: Index): Double =
      if (n % 2 == 0) Vector(a, b, c)(n / 2)
      else side(lengthFun(n - 1), lengthFun(n + 1), lengthFun(n + 3))

    def lengthFun(n: Int): Double = sideLengthFun(mod6(n))

    lazy val sideLength = (0 to 5).map(j => j -> sideLengthFun(j)).toMap

    def length(n: Int) = sideLength(mod6(n))

    def getArcLength(
        i: Int,
        j: Int,
        initShift: Double,
        lastShift: Double
    ): Double = {
      assert(i < j && j < i + 4, s"getArcLength called with $i, $j")
      val init = length(i) - initShift
      val last = lastShift
      j - i match {
        case 1 => distance2(init, last)
        case 2 => distance3(init, length(i + 1), last)
        case 3 => distance4(init, length(i + 1), length(i + 2), last)
      }
    }

    def arcLength(
        i: Index,
        j: Index,
        initShift: Double,
        lastShift: Double
    ): Double =
      if (i == j) abs(lastShift - initShift)
      else if (i > j) arcLength(j, i, lastShift, initShift)
      else if (j - i < 4) getArcLength(i, j, initShift, lastShift)
      else getArcLength(j, i + 6, lastShift, initShift)
  }
}

/**
  * An edge that is the seam of a pair of pants
  *
  * @param pants the index of the pair of pants of which this is a seam
  * @param initial the initial vertex
  * @param terminal the terminal vertex
  * @param positivelyOriented whether the edge is positively oriented
  */
case class PantsSeam(
    pants: Index,
    initial: Vertex,
    terminal: Vertex,
    positivelyOriented: Boolean
) extends OrientedEdge {
  lazy val flip = PantsSeam(pants, terminal, initial, !positivelyOriented)
}

object PantsSeam {
  import SkewCurveEdge.close
  def compareSeamPoints(
      ps1: PantsSeam,
      pos1: Double,
      ps2: PantsSeam,
      pos2: Double,
      len: Double
  ): Boolean = {
    require((ps1 == ps2) || (ps1 == ps2.flip))
    if (ps1 == ps2) close(pos1, pos2)
    else close(pos1+ pos2, len)
  }
}

/**
  * A curve in the curve system (i.e., the collection of terms) along which we split to get the pants decomposition.
  * This is a quotient of two pants boundaries that have been identified, with orientation determining one as on the left.
  *
  * @param left the pants boundary on the left.
  * @param right the pants boundary on the right.
  */
case class Curve(left: PantsBoundary, right: PantsBoundary) {

  /**
    * the pants boundaries identified to get this curve.
    */
  val support: Set[PantsBoundary] = Set(left, right)

  /**
    * (indices of) adjacent pairs of pants
    */
  val neighbours: Set[Index] = support.map(_.pants)

  /**
    * whether a pants boundary is contained in the curve
    *
    * @param pb a pants boundary
    * @return boolean for containment
    */
  def contains(pb: PantsBoundary): Boolean = support.contains(pb)

  /**
    * Curve in curve system after a pair of pants is deleted
    *
    * @param n the index of the deleted pants
    * @return optionally the curve with indices shifted; nothing if curve in deleted pants
    */
  def dropOpt(n: Index): Option[Curve] =
    for {
      newLeft <- left.dropOpt(n)
      newRight <- right.dropOpt(n)
    } yield Curve(newLeft, newRight)
}

import SkewCurve._

/**
  * A curve in the curve system for a pants decomposition for a hyperbolic surface obtained by twisting and regluing along the pants system
  * Twists and vertex positions are parametrized as fractions of the length, so are in [0, 1].
  *
  * Generically, there are 4 vertices, two at 0 and 0.5 and the other two at `twist` and `twist + 0.5 (mod 1)`.
  *
  * @param left the pants boundary to the left
  * @param right the pants boundary to the right
  * @param twist the twist, parametrized as a fraction of the length, so is in [0, 1]
  * @param length the length of the curve in the hyperbolic structure.
  */
case class SkewCurve(
    left: PantsBoundary,
    right: PantsBoundary,
    twist: BigDecimal,
    length: BigDecimal
) { curve => // `curve` gives this curve

  /**
    * the corresponding curve in the untwisted surface
    *
    */
  def base = Curve(left, right)

  /**
    * whether the vertices and edges of the two pants match, either directly or with exchange;
    * this also determines whether the number of vertices is 2 (otherwise it is 4)
    */
  val skewLess: Boolean = twist == 0 || twist == 0.5

  /**
    * the pants boundaries identified to get this curve.
    */
  val support: Set[PantsBoundary] = Set(left, right)

  /**
    * (indices of) adjacent pairs of pants
    */
  val neighbours: Set[Index] = support.map(_.pants)

  /**
    * the position (in [0, 1]) of the next vertex after 0; the next vertex after 0.5 is `0.5 + shift`.
    */
  val shift = if (twist <= mod1(twist + 0.5)) twist else mod1(twist + 0.5)

  /**
    * whether a pants boundary is contained in the curve
    *
    * @param pb a pants boundary
    * @return boolean for containment
    */
  def contains(pb: PantsBoundary): Boolean = support.contains(pb)

  /**
    * the postion of the next vertex (in [0, 1))
    *
    * @param position position of the given vertex
    * @return position of the next vertex
    */
  def nextVertex(position: BigDecimal): BigDecimal =
    if (skewLess) {
      assert(
        position == 0 || position == 0.5,
        s"twist is $twist (hence untwisted) but vertex at $position"
      )
      mod1(position + 0.5)
    } else if (position == 0 || position == 0.5) position + shift
    else if (position < 0.5) 0.5
    else 0

  /**
    * the postion of the previous vertex (in [0, 1))
    *
    * @param position position of the given vertex
    * @return position of the previous vertex
    */
  def previousVertex(position: BigDecimal): BigDecimal =
    if (skewLess) {
      assert(
        position == 0 || position == 0.5,
        s"twist is $twist (hence untwisted) but vertex at $position"
      )
      mod1(position + 0.5)
    } else if (position == 0) 0.5 + shift
    else if (position == 0.5) shift
    else if (position > 0.5) 0.5
    else 0

  /**
    * position of the next vertex along the orientation
    *
    * @param position position of the present vertex
    * @param positivelyOriented whether orientation is poistive
    * @return position of the next vertex along the orientation
    */
  def shiftedVertex(position: BigDecimal, positivelyOriented: Boolean) =
    if (positivelyOriented) nextVertex(position) else (previousVertex(position))

  /**
    * edges between the given vertex and the opposite one along the given orientation;
    * a single edge if untwisted and two edges if twisted
    *
    * @param position
    * @param positivelyOriented
    * @return
    */
  def edgesToOppositeVertex(
      position: BigDecimal,
      positivelyOriented: Boolean
  ): Vector[Edge] =
    if (skewLess) Vector(SkewCurveEdge(curve, position, positivelyOriented))
    else
      Vector(
        SkewCurveEdge(curve, position, positivelyOriented),
        SkewCurveEdge(
          curve,
          shiftedVertex(position, positivelyOriented),
          positivelyOriented
        )
      )

  /**
    * vetices on edges between the given vertex and the opposite vertex along the given orientation;
    * we have a single edge, hence two vertices, if untwisted and two edges, hence three vertices, if twisted
    *
    * @param position
    * @param positivelyOriented
    * @return
    */
  def verticesToOppositeVertex(
      position: BigDecimal,
      positivelyOriented: Boolean
  ): Set[Vertex] =
    if (skewLess)
      Set(
        SkewCurveVertex(curve, position),
        SkewCurveVertex(curve, shiftedVertex(position, positivelyOriented))
      )
    else
      Set(
        SkewCurveVertex(curve, position),
        SkewCurveVertex(curve, shiftedVertex(position, positivelyOriented)),
        SkewCurveVertex(
          curve,
          shiftedVertex(
            shiftedVertex(position, positivelyOriented),
            positivelyOriented
          )
        )
      )

  /**
    * position of the first vertex on the given pants boundary in the curve after gluing with a twist
    *
    * @param left whether we consider the pants
    * @param top whether we consider the top edge
    * @return position
    */
  def initPos(left: Boolean, top: Boolean) =
    if (left) {
      if (top) BigDecimal(0) else BigDecimal(0.5)
    } else {
      if (top) mod1(BigDecimal(0.5) + twist) else twist
    }

  /**
    * the edges in the twisted pants complex on an edge in a pants boundary adjacent to the given curve;
    * a pair of edges if twisted, otherwise a single edge
    *
    * @param left whether the pants we consider are on the left
    * @param top whether we consider the top edge
    * @return edges in the twisted pants complex
    */
  def edgesOn(left: Boolean, top: Boolean): Vector[Edge] =
    edgesToOppositeVertex(initPos(left, top), left)

  /**
    * the vertices in the twisted pants complex on an edge in a pants boundary adjacent to the given curve;
    * three vertices if twisted, otherwise two vertices
    *
    * @param left whether the pants we consider are on the left
    * @param top whether we consider the top edge
    * @return vertices in the twisted pants complex
    */
  def verticesOn(left: Boolean, top: Boolean): Set[Vertex] = {
    verticesToOppositeVertex(initPos(left, top), left)
  }

}

object SkewCurve {
  @annotation.tailrec
  def mod1(x: BigDecimal): BigDecimal =
    if (x >= 1)(mod1(x - 1))
    else if (x >= 0) x
    else (mod1(x + 1))

  /**
    * untwisted skew curve corresponding to a curve in a pants decomposition
    *
    * @param c the original curve
    * @param length the length in the hyperbolic structure
    * @return skew curve with given length and no twist
    */
  def untwisted(c: Curve, length: BigDecimal = 1) =
    SkewCurve(c.left, c.right, 0, length)

  /**
    * enumerate twisted curves
    *
    * @param c the base curve to use
    * @param twists a vector of twists
    * @param lengths a vector of lengths
    * @return a vector of twisted curves from the base curve with all given lengths and twists
    */
  def enumerate(
      c: Curve,
      twists: Vector[BigDecimal],
      lengths: Vector[BigDecimal]
  ): Vector[SkewCurve] =
    for {
      t <- twists
      l <- lengths
    } yield SkewCurve(c.left, c.right, t, l)

  /**
    * enumerate collections of twisted curves; giving a vector of vectors
    * the outer vector entries are for different twists and lengths
    *
    * @param cs the collection of base curves to use
    * @param twists a vector of twists
    * @param lengths a vector of lengths
    * @return a vector, each component of which is twisted forms of the given vector of base curves;
    */
  def polyEnumerate(
      cs: Vector[Curve],
      twists: Vector[BigDecimal],
      lengths: Vector[BigDecimal]
  ): Vector[Vector[SkewCurve]] =
    cs match {
      case Vector() => Vector(Vector())
      case x +: ys =>
        for {
          c <- enumerate(x, twists, lengths)
          tcs <- polyEnumerate(ys, twists, lengths)
        } yield c +: tcs
    }

  implicit val rw: RW[SkewCurve] = macroRW
}

/**
  * vertex on a curve
  *
  * @param curve the curve
  * @param first whether this is the first vertex
  */
case class CurveVertex(curve: Curve, first: Boolean) extends Vertex

/**
  * vertex on a skew curve
  *
  * @param curve the curve
  * @param position position on the curve in [0, 1)
  */
case class SkewCurveVertex(curve: SkewCurve, position: BigDecimal)
    extends Vertex

/**
  * An edge on a curve
  *
  * @param curve the curve on which the edge lies
  * @param top whether this is the top edge
  * @param positivelyOriented whether the edge is positively oriented
  */
case class CurveEdge(curve: Curve, top: Boolean, positivelyOriented: Boolean)
    extends OrientedEdge {
  lazy val flip = CurveEdge(curve, top, !positivelyOriented)

  lazy val initial: Vertex =
    CurveVertex(curve, !(positivelyOriented ^ top)) // is the first vertex for the top edge with positive orientation and the bottom with negative orientation

  lazy val terminal: Vertex =
    CurveVertex(curve, positivelyOriented ^ top) // the vertex different from the initial vertex
}

/**
  * An edge on a skew-curve, i.e., curve in a pants decomposition glued with a twist
  *
  * @param curve the skew-curve in the pants decomposition
  * @param initialPosition the position of the initial vertex
  * @param positivelyOriented whether the edge is positively oriented
  */
case class SkewCurveEdge(
    curve: SkewCurve,
    initialPosition: BigDecimal,
    positivelyOriented: Boolean
) extends OrientedEdge {

  /**
    * the position of the final vertex
    */
  lazy val finalPosition =
    if (positivelyOriented) curve.nextVertex(initialPosition)
    else curve.previousVertex(initialPosition)

  /**
    * length of the edge
    *
    * @return the length
    */
  lazy val length =
    curve.length * (if (positivelyOriented)
                      mod1(finalPosition - initialPosition)
                    else mod1(initialPosition - finalPosition))

  lazy val flip = SkewCurveEdge(curve, finalPosition, !positivelyOriented)

  lazy val initial = SkewCurveVertex(curve, initialPosition)

  lazy val terminal = SkewCurveVertex(curve, finalPosition)

}

object SkewCurveEdge {
  val tolerance = math.pow(10, -6)
  def close(x: Double, y: Double) = math.abs(x - y) < tolerance

  def comparePoints(
      edge1: SkewCurveEdge,
      disp1: Double,
      edge2: SkewCurveEdge,
      disp2: Double
  ): Boolean = {
    require((edge1 == edge2) || (edge1 == edge2.flip))
    if (edge1 == edge2) close(disp1, disp2)
    else close(disp1 + disp2, edge1.length.toDouble)
  }
}

/**
  * Hexagon in a pair of pants
  *
  * @param pants the index of the pair of pants
  * @param top whether this is the top face
  * @param cs system of curves giving the pants decomposition
  */
case class PantsHexagon(pants: Index, top: Boolean, cs: Set[Curve])
    extends Hexagon {
  val vertices: Set[Vertex] =
    for {
      direction: Z3 <- Z3.enum.toSet
      first <- Set(true, false)
    } yield vertex(PantsBoundary(pants, direction), first, cs)

  private val enum = if (top) Z3.enum else Z3.flipEnum
  val boundary: Vector[Edge] =
    for {
      direction <- enum
      e <- Vector(
        edge(
          PantsBoundary(pants, direction),
          top,
          positivelyOriented = top,
          cs
        ),
        seam(pants, direction, cs, top)
      )
    } yield e

}

/**
  * "hexagon" in a skew pair of pants; note that this is not in general a hexagon as some of the edges are subdivided
  *
  * @param pants the index of the pair of (skew) pants
  * @param top whether this is the top face
  * @param cs system of curves giving the pants decomposition
  */
case class SkewPantsHexagon(pants: Index, top: Boolean, cs: Set[SkewCurve])
    extends Polygon {
  lazy val vertices: Set[Vertex] =
    Z3.enum.toSet.flatMap { direction: Z3 =>
      skewVertices(PantsBoundary(pants, direction), top, cs)
    }

  lazy val segments: Vector[Vector[Edge]] = {
    if (top) {
      Z3.enum.map { direction: Z3 =>
        skewEdges(
          PantsBoundary(pants, direction),
          top,
          positivelyOriented = top,
          cs
        )
      }
    } else {
      Z3.flipEnum.map { direction: Z3 =>
        skewEdges(
          PantsBoundary(pants, direction),
          top,
          positivelyOriented = top,
          cs
        )
      }
    }

    
  }

  lazy val topHex = Hexagon
        .Hyperbolic(
          edgeLengths(0).get,
          edgeLengths(1).get,
          edgeLengths(2).get
        )

  lazy val bottomHex = Hexagon
        .Hyperbolic(
          edgeLengths(0).get,
          edgeLengths(2).get,
          edgeLengths(1).get
        )

  lazy val boundary = fillSeams(pants, segments, top)
  lazy val sides = boundary.size

  /**
    * Optional lengths of edges, None if a boundary edge
    *
    * @return
    */
  lazy val edgeLengths: Vector[Option[Double]] =
    Z3.enum.map { direction: Z3 =>
      getSkewCurve(PantsBoundary(pants, direction), cs)
        .map {
          case (curve, left) => (curve.length.toDouble) / 2
        }
    }

  def sideLength(edge: Edge): Double = {
    require(
      boundary.forall(e => !e.isInstanceOf[BoundaryEdge]),
      "Cannot assign lengths to all sides if one is a boundaryedge"
    )
    edge match {
      case s: SkewCurveEdge => s.length.toDouble
      case p: PantsSeam     => seamLengthMap(p)
      case _                => 0
    }
  }

  lazy val seamAndLength: Vector[(PantsSeam, Double)] = {
    require(edgeLengths.forall(x => x.isDefined))
    if (top) {
      for (i <- Vector(0, 1, 2)) yield {
        (
          PantsSeam(
            pants,
            skewEdges(PantsBoundary(pants, Z3(i)), top, top, cs).last.terminal,
            skewEdges(PantsBoundary(pants, Z3((i + 1) % 3)), top, top, cs).head.initial,
            top
          ),
          Hexagon.side(
            edgeLengths(i).getOrElse(0),
            edgeLengths((i + 1) % 3).getOrElse(0),
            edgeLengths((i + 2) % 3).getOrElse(0)
          )
        )
      }
    } else {
      for (i <- Vector(0, 2, 1)) yield {
        (
          PantsSeam(
            pants,
            skewEdges(PantsBoundary(pants, Z3(i)), top, top, cs).last.terminal,
            skewEdges(PantsBoundary(pants, Z3((i + 2) % 3)), top, top, cs).head.initial,
            top
          ),
          Hexagon.side(
            edgeLengths(i).getOrElse(0),
            edgeLengths((i + 2) % 3).getOrElse(0),
            edgeLengths((i + 1) % 3).getOrElse(0)
          )
        )
      }
    }
  }

  lazy val seamLengthMap = seamAndLength.toMap
}

object SkewPantsHexagon {
  def displacementFromPantsBoundaryVertex(
      sph: SkewPantsHexagon,
      edge: SkewCurveEdge,
      initialDisplacement: Double
  ): Double = sph.boundary.indexOf(edge) match {
    case 0 => initialDisplacement
    case _ =>
      sph.boundary(sph.boundary.indexOf(edge) - 1) match {
        case b: BoundaryEdge  => initialDisplacement
        case s: SkewCurveEdge => (initialDisplacement + s.length.doubleValue)
        case p: PantsSeam     => initialDisplacement
      }
  }
  def skewIndexToHexagonIndex(sph: SkewPantsHexagon, n: Index): Index = {
    require(n < sph.sides)
    n match {
      case 0 => 0
      case _ =>
        sph.boundary(n) match {
          case b: BoundaryEdge => (skewIndexToHexagonIndex(sph, n - 1) + 1)
          case p: PantsSeam    => (skewIndexToHexagonIndex(sph, n - 1) + 1)
          case s: SkewCurveEdge =>
            sph.boundary(n - 1) match {
              case b: BoundaryEdge  => (skewIndexToHexagonIndex(sph, n - 1) + 1)
              case p: PantsSeam     => (skewIndexToHexagonIndex(sph, n - 1) + 1)
              case s: SkewCurveEdge => skewIndexToHexagonIndex(sph, n - 1)
            }
        }
    }
  }
  def getSeamLength(sph: SkewPantsHexagon, ps: PantsSeam): Double = {
    sph.seamLengthMap(ps)
  }

  def adjacentSkewCurveEdges(face: Polygon, i1: Index, i2: Index): Boolean = {
    require(face.isInstanceOf[SkewPantsHexagon])
    if (math.abs(i1 - i2) != 1) false
    else {
      face.boundary(i1) match {
        case s: SkewCurveEdge =>
          face.boundary(i2) match {
            case s: SkewCurveEdge => true
            case _                => false
          }
        case _ => false
      }
    }
  }

  implicit val rw: RW[SkewPantsHexagon] = macroRW
}

case class SkewPantsSurface(numPants: Index, cs: Set[SkewCurve])
    extends PureTwoComplex[SkewPantsHexagon] {
  lazy val indices: Vector[Index] = (0 until numPants).toVector

  lazy val faceVector: Vector[SkewPantsHexagon] =
    for {
      pants: Index <- indices
      top <- Vector(true, false)
    } yield SkewPantsHexagon(pants, top, cs)

  lazy val faces = faceVector.toSet

  lazy val fundamentalClass = {
    val cv = faces.toVector.collect {
      case ph: SkewPantsHexagon => (ph: Polygon, if (ph.top) 1 else -1)
    }
    FormalSum.reduced(cv)
  }
}

object SkewPantsSurface {

  /**
    * untwisted "skew pants surface" from a pants surface
    *
    * @param surf the original surface with a pants decomposition
    * @param m lengths of curves
    * @return skew pant surface without twists
    */
  def untwisted(surf: PantsSurface, m: Map[Curve, BigDecimal] = Map()) = {
    def l(c: Curve) = m.getOrElse(c, BigDecimal(0))
    val cs = surf.cs.map(c => SkewCurve.untwisted(c, l(c)))
    SkewPantsSurface(surf.numPants, cs)
  }

  /**
    * all hyperbolic (skew) surfaces with pants decomposition, starting from a surface with pants decompostion with twists and lengths specified
    *
    * @param surf the base surface with pants decomposition
    * @param twists the collection of twists to use
    * @param lengths the collection of lengths to use
    * @return
    */
  def enumerate(
      surf: PantsSurface,
      twists: Vector[BigDecimal],
      lengths: Vector[BigDecimal] = Vector(1)
  ) =
    SkewCurve.polyEnumerate(surf.cs.toVector, twists, lengths).map { tcs =>
      SkewPantsSurface(surf.numPants, tcs.toSet)
    }

  implicit val rw: RW[SkewPantsSurface] = macroRW
}

/**
  * Surface with a pants decomposition (compact, possibly with boundary and possibly disconnected)
  *
  * @param numPants the number of pairs of pants
  * @param cs the curve system giving the pants decomposition
  */
case class PantsSurface(numPants: Index, cs: Set[Curve])
    extends PureTwoComplex[PantsHexagon] {
  val indices: Vector[Index] = (0 until numPants).toVector

  val faces: Set[PantsHexagon] =
    for {
      pants: Index <- indices.toSet
      top <- Set(true, false)
    } yield PantsHexagon(pants, top, cs)

  val topFaces: Vector[PantsHexagon] = faces.toVector.collect {
    case ph: PantsHexagon if ph.top => ph
  }

  val fundamentalClass: FormalSum[Polygon] = {
    val cv = faces.toVector.collect {
      case ph: PantsHexagon => (ph: Polygon, if (ph.top) 1 else -1)
    }
    FormalSum.reduced(cv)
  }

  val allCurves: Set[PantsBoundary] =
    for {
      direction: Z3 <- Z3.enum.toSet
      pants <- indices
    } yield PantsBoundary(pants, direction)

  val csSupp: Set[PantsBoundary] = cs.flatMap(_.support)

  /**
    * curves in the boundary; we can attach other pairs of pants to these.
    */
  val boundaryCurves: Set[PantsBoundary] = allCurves -- csSupp

  /**
    * indices of pairs of pants for which two boundary components are glued to each other, i.e., which contain a loop.
    */
  val loopIndices: Set[Index] =
    cs.collect {
      case p if p.left.pants == p.right.pants => p.left.pants
    }

  /**
    * indices of pants that intersect the boundary of the surface
    */
  val boundaryIndices: Set[Index] = boundaryCurves.map(_.pants)

  /**
    * whether the surface is closed
    *
    * @return boolean for surface being closed
    */
  def isClosed: Boolean = boundaryIndices.isEmpty

  /**
    * number of curves in the boundary of a pair of pants that are interior to the surface
    *
    * @param index
    */
  def innerCurves(index: Index): Int =
    csSupp.count((p) => p.pants == index)

  /**
    * surface obtained by deleting a pair of pants
    *
    * @param n
    * @return
    */
  def drop(n: Index): PantsSurface =
    PantsSurface(numPants - 1, cs.flatMap(_.dropOpt(n)))

  /**
    * pants neighbouring a given set
    *
    * @param pantSet set whose neighbourhood is sought
    * @return indices of the neighbouring pants
    */
  def neighbourhood(pantSet: Set[Index]): Set[Index] =
    indices
      .filter(
        (m) =>
          cs.exists(
            (curve) =>
              curve.neighbours
                .contains(m) && curve.neighbours.intersect(pantSet).nonEmpty
          )
      )
      .toSet

  /**
    * connected component given a set of pants (should start with a single one)
    *
    * @param pantSet starting set of pants
    * @return set of indices for the component
    */
  @annotation.tailrec
  final def component(pantSet: Set[Index]): Set[Index] = {
    val expand = neighbourhood(pantSet)
    if (expand == pantSet) expand
    else component(expand)
  }

  /**
    * whether the surface is connected
    *
    * @return connectivity
    */
  lazy val isConnected: Boolean =
    (numPants <= 1) || (component(Set(0)) == indices.toSet)

  /**
    * peripheral pairs of pants, i.e., so that the complement of the pants is connected.
    *
    * @return indices of peripheral pants
    */
  lazy val peripheral: Set[Index] =
    indices.filter((m) => drop(m).isConnected).toSet

  /**
    * attach a new pair of pants along one specified boundary component
    *
    * @param pb pants boundary of the given surface along which to attach the new pants
    * @return pants surface with new pants attached
    */
  def glue1(pb: PantsBoundary) =
    PantsSurface(numPants + 1, cs + Curve(pb, PantsBoundary(numPants, Z3(0))))

  /**
    * attach a new pair of pants along two specified boundary components
    *
    * @param pb1 first pants boundary of the given surface along which to attach the new pants
    * @param pb2 second pants boundary of the given surface along which to attach the new pants
    * @return pants surface with new pants attached
    */
  def glue2(pb1: PantsBoundary, pb2: PantsBoundary) =
    PantsSurface(
      numPants + 1,
      cs union Set(
        Curve(pb1, PantsBoundary(numPants, Z3(0))),
        Curve(pb2, PantsBoundary(numPants, Z3(1)))
      )
    )

  /**
    * attach a new pair of pants along three specified boundary components
    *
    * @param pb1 first pants boundary of the given surface along which to attach the new pants
    * @param pb2 second pants boundary of the given surface along which to attach the new pants
    * @param pb3 third pants boundary of the given surface along which to attach the new pants
    * @return pants surface with new pants attached
    */
  def glue3(pb1: PantsBoundary, pb2: PantsBoundary, pb3: PantsBoundary) =
    PantsSurface(
      numPants + 1,
      cs union Set(
        Curve(pb1, PantsBoundary(numPants, Z3(0))),
        Curve(pb2, PantsBoundary(numPants, Z3(1))),
        Curve(pb3, PantsBoundary(numPants, Z3(2)))
      )
    )

  /**
    * attach a new pair of pants along one specified boundary component, with the other two boundary components of the new pants glued to each other.
    *
    * @param pb pants boundary of the given surface along which to attach the new pants
    * @return pants surface with new pants attached
    */
  def glueLoop(pb: PantsBoundary) =
    PantsSurface(
      numPants + 1,
      cs union Set(
        Curve(pb, PantsBoundary(numPants, Z3(0))),
        Curve(PantsBoundary(numPants, Z3(1)), PantsBoundary(numPants, Z3(2)))
      )
    )

  /**
    * attach new pants along a single boundary component in all possible ways
    *
    * @return set of pants surfaces obtained by attachment
    */
  def allGlue1: Set[PantsSurface] = boundaryCurves.map(glue1)

  /**
    * attach new pants along a single boundary component, with the other two components of the new pants glued, in all possible ways
    *
    * @return set of pants surfaces obtained by attachment
    */
  def allGlueLoop: Set[PantsSurface] = boundaryCurves.map(glueLoop)

  /**
    * attach new pants along two boundary components in all possible ways
    *
    * @return set of pants surfaces obtained by attachment
    */
  def allGlue2: Set[PantsSurface] =
    for {
      pb1 <- boundaryCurves
      pb2 <- boundaryCurves
      if pb2 < pb1
    } yield glue2(pb1, pb2)

  /**
    * attach new pants along three boundary components in all possible ways
    *
    * @return set of pants surfaces obtained by attachment
    */
  def allGlue3: Set[PantsSurface] =
    for {
      pb1 <- boundaryCurves
      pb2 <- boundaryCurves
      if pb1 < pb2
      pb3 <- boundaryCurves
      if pb2 < pb3
    } yield glue3(pb1, pb2, pb3)

  /**
    * attach new pants along a positive number of boundary components in all possible ways
    *
    * @return set of pants surfaces obtained by attachment
    */
  def allGlued: Set[PantsSurface] =
    allGlue1 union allGlue2 union allGlue3 union allGlueLoop

}

object PantsSurface {

  /**
    * Bers constant - upper bound on the length of pants in a minimal pants decomposition
    *
    * @param g the genus
    * @return the Bers upper bound
    */
  def bers(g: Int) = 26 * (g - 1)

  /**
    * Margulis constant
    */
  val margulis =
    2* Hexagon.arcsinh(sqrt((2 * cos(2 * Pi / 7) - 1) / (8 * cos(Pi / 7) + 7)))

  /**
    * determine whether the pants decompositions are isomorphic
    *
    * @param first the first pants decomposition
    * @param second the second pants decomposition
    * @return boolean : whether isomorphic
    */
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
            val secondIndices = second.boundaryIndices.filter(
              (n) => second.innerCurves(n) == first.innerCurves(ind)
            )
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

  /**
    * vector of surfaces with duplicates up to isomorphism purged
    *
    * @param surfaces an initial collection of surfaces
    * @return vector of isomorphism class representatives for the original collection
    */
  def distinct(surfaces: Vector[PantsSurface]): Vector[PantsSurface] =
    surfaces match {
      case Vector()         => Vector()
      case head +: Vector() => Vector(head)
      case head +: tail =>
        val newTail = distinct(tail)
        if (newTail.exists(isomorphic(_, head))) newTail
        else head +: newTail
    }

  /**
    * lazy list of vectors of all connected surfaces, possibly with boundary, with a given number of pants
    */
  val all: LazyList[Vector[PantsSurface]] = LazyList.from(0).map(getAll)

  /**
    * determine isomorophism classes of connected pants decompositions recursively
    *
    * @param n number of pairs of pants
    * @return Vector of representatives of isomorphism classes of connected pants decompositions
    */
  def getAll(n: Int): Vector[PantsSurface] =
    if (n == 0) Vector()
    else if (n == 1)
      Vector(
        PantsSurface(1, Set()),
        PantsSurface(
          1,
          Set(Curve(PantsBoundary(0, Z3(0)), PantsBoundary(0, Z3(1))))
        )
      )
    else
      distinct(
        all(n - 1).flatMap(
          (s) => s.allGlued.toVector
        )
      )

  /**
    * lazy list of vectors of all closed surfaces, with a given number of pants
    */
  val allClosed: LazyList[Vector[PantsSurface]] = all.map(_.filter(_.isClosed))

  /**
    * given a pants boundary, returns a curve if any that contains this (each curve is obtained by identifying two pants boundaries),
    * and whether this is on the left
    *
    * @param pb the pants boundary
    * @param cs the curve system
    * @return optionally pair consisting of a curve and a boolean (whether on left)
    */
  def getCurve(pb: PantsBoundary, cs: Set[Curve]): Option[(Curve, Boolean)] =
    cs.find(
        (c) => c.left == pb
      )
      .map((c) => c -> true)
      .orElse(
        cs.find(
            (c) => c.right == pb
          )
          .map((c) => c -> false)
      )

  /**
    * given a pants boundary, returns a skew curve if any that contains this (each curve is obtained by identifying two pants boundaries),
    * and whether this is on the left
    *
    * @param pb the pants boundary
    * @param cs the curve system
    * @return optionally pair consisting of a skew curve and a boolean (whether on left)
    */
  def getSkewCurve(
      pb: PantsBoundary,
      cs: Set[SkewCurve]
  ): Option[(SkewCurve, Boolean)] =
    cs.find(
        (c) => c.left == pb
      )
      .map((c) => c -> true)
      .orElse(
        cs.find(
            (c) => c.right == pb
          )
          .map((c) => c -> false)
      )

  /**
    * given an edge in a pants boundary, returns an edge in the two complex from the pants decomposition correspoding to this (image in the quotient)
    *
    * @param pb the pants boundary
    * @param top whether this is the top edge
    * @param positivelyOriented whether the edge is positively oriented
    * @param cs the curve system
    * @return edge in the quotient two complex
    */
  def edge(
      pb: PantsBoundary,
      top: Boolean,
      positivelyOriented: Boolean,
      cs: Set[Curve]
  ): Edge =
    getCurve(pb, cs)
      .map {
        case (curve, positivelyOriented) =>
          CurveEdge(curve, top, positivelyOriented)
      }
      .getOrElse(BoundaryEdge(pb, top, positivelyOriented))

  /**
    * given a vertex in a pants boundary, returns a vertex in the two complex from the pants decomposition correspoding to this (image in the quotient)
    *
    * @param pb the pants boundary
    * @param first whether this is the first vertex
    * @param cs the curve sysem
    * @return vertex in the quotient two complex
    */
  def vertex(pb: PantsBoundary, first: Boolean, cs: Set[Curve]): Vertex =
    getCurve(pb, cs)
      .map {
        case (curve, positivelyOriented) =>
          CurveVertex(curve, !(first ^ positivelyOriented))
      }
      .getOrElse(BoundaryVertex(pb, first))

  /**
    * seam as an edge in the two complex from the pants decomposition
    *
    * @param pants index of pants
    * @param direction direction of the (initial point of the) seam
    * @param cs the curve system
    * @param top whether the seam is from the top face
    * @return edge in the two complex
    */
  def seam(
      pants: Index,
      direction: Z3,
      cs: Set[Curve],
      top: Boolean
  ): PantsSeam = {
    val initial = vertex(PantsBoundary(pants, direction), first = !top, cs)
    val terminal =
      vertex(
        PantsBoundary(pants, if (top) direction.next else direction.prev),
        first = top,
        cs
      )
    PantsSeam(pants, initial, terminal, top)
  }

  /**
    * given an edge in a pants boundary, returns a vector ofedges in the two complex from the skew pants decomposition
    * correspoding to this (image in the quotient); the vector has one or two edges
    *
    * @param pb the pants boundary
    * @param top whether this is the top edge
    * @param positivelyOriented whether the edge is positively oriented
    * @param cs the curve system
    * @return edge in the quotient two complex
    */
  def skewEdges(
      pb: PantsBoundary,
      top: Boolean,
      positivelyOriented: Boolean,
      cs: Set[SkewCurve]
  ): Vector[Edge] =
    getSkewCurve(pb, cs)
      .map {
        case (curve, left) => curve.edgesOn(left, top)
      }
      .getOrElse(Vector(BoundaryEdge(pb, top, positivelyOriented)))

  /**
    * given an edge in a pants boundary, returns a vector of the vertices in the two complex from the skew pants decomposition
    * contained in the image of the edge
    *
    * @param pb the pants boundary
    * @param first whether this is the first vertex
    * @param cs the curve sysem
    * @return vector of vertices in the quotient two complex
    */
  def skewVertices(
      pb: PantsBoundary,
      top: Boolean,
      cs: Set[SkewCurve]
  ): Set[Vertex] =
    getSkewCurve(pb, cs)
      .map {
        case (curve, left) => curve.verticesOn(left, top)
      }
      .getOrElse(Set(true, false).map(BoundaryVertex(pb, _)))

  /**
    * recursively fill in seams in a SkewPantsHexagon given collections of edges that are in a fixed SkewPantsHexagon
    *
    * @param pants index of pants containing the edges
    * @param top whether the hexagon is the top face
    * @param segments the collections of sequences of edges
    * @param gapLess the sequence of edges obtained so far without gaps
    * @return sequence of edges with gaps filled in by seams
    */
  def fillSeamsRec(
      pants: Index,
      top: Boolean,
      segments: Vector[Vector[Edge]],
      gapLess: Vector[Edge]
  ): Vector[Edge] =
    segments match {
      case Vector() => gapLess
      case ys :+ x =>
        val newSeam =
          PantsSeam(pants, x.last.terminal, gapLess.head.initial, top)
        fillSeamsRec(pants, top, ys, x ++ (newSeam +: gapLess))
    }

  /**
    * fill in seams in a SkewPantsHexagon given collections of edges that are in a fixed SkewPantsHexagon
    *
    * @param pants index of pants containing the edges
    * @param segments the collections of sequences of edges
    * @param top whether the hexagon is the top face
    * @return sequence of edges with gaps filled in by seams
    */
  def fillSeams(pants: Index, segments: Vector[Vector[Edge]], top: Boolean) =
    fillSeamsRec(
      pants,
      top,
      segments.init,
      segments.last :+ (
        PantsSeam(
          pants,
          segments.last.last.terminal,
          segments.head.head.initial,
          top
        )
      )
    )
}
