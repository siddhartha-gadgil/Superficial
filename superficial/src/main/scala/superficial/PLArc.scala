package superficial
import scala.collection.parallel._, immutable.ParSet

import Polygon.Index

case class PLArc(
    base: NormalArc[SkewPantsHexagon],
    initialDisplacement: BigDecimal,
    finalDisplacement: BigDecimal
) {
  // require(!( base.face.boundary(base.initial).isInstanceOf[BoundaryEdge] || base.face.boundary(base.terminal).isInstanceOf[BoundaryEdge] ))
  val hexagonInitialDisplacement: Option[Double] =
    base.face.boundary(base.initial) match {
      case b: BoundaryEdge => None
      case s: SkewCurveEdge =>
        Some(
          SkewPantsHexagon.DisplacementFromPBVertex(
            base.face,
            s,
            initialDisplacement.doubleValue
          )
        )
      case p: PantsSeam => Some(initialDisplacement.doubleValue)
    }
  val hexagonFinalDisplacement: Option[Double] =
    base.face.boundary(base.terminal) match {
      case b: BoundaryEdge => None
      case s: SkewCurveEdge =>
        Some(
          SkewPantsHexagon.DisplacementFromPBVertex(
            base.face,
            s,
            finalDisplacement.doubleValue
          )
        )
      case p: PantsSeam => Some(finalDisplacement.doubleValue)
    }
  val length: Double = {
    require(
      base.face.edgeLengths
        .forall(x => x.isDefined) && hexagonInitialDisplacement.isDefined && hexagonFinalDisplacement.isDefined
    )
    if (base.face.top) {
      Hexagon
        .Hyperbolic(
          base.face.edgeLengths(0).get,
          base.face.edgeLengths(1).get,
          base.face.edgeLengths(2).get
        )
        .arcLength(
          SkewPantsHexagon.SkewIndexToHexagonIndex(base.face, base.initial),
          SkewPantsHexagon.SkewIndexToHexagonIndex(base.face, base.terminal),
          hexagonInitialDisplacement.get,
          hexagonFinalDisplacement.get
        )
    } else {
      Hexagon
        .Hyperbolic(
          base.face.edgeLengths(0).get,
          base.face.edgeLengths(2).get,
          base.face.edgeLengths(1).get
        )
        .arcLength(
          SkewPantsHexagon.SkewIndexToHexagonIndex(base.face, base.initial),
          SkewPantsHexagon.SkewIndexToHexagonIndex(base.face, base.terminal),
          hexagonInitialDisplacement.get,
          hexagonFinalDisplacement.get
        )
    }
  }
}

object PLArc {
  def freeEnumerate(
      arc: NormalArc[SkewPantsHexagon],
      sep: BigDecimal
  ): ParSet[PLArc] = {
    require(
      !(arc.terminalEdge.isInstanceOf[BoundaryEdge] || arc.initialEdge
        .isInstanceOf[BoundaryEdge])
    )
    arc.initialEdge match {
      case e1: SkewCurveEdge =>
        arc.terminalEdge match {
          case e2: SkewCurveEdge =>
            for (d1: BigDecimal <- BigDecimal(0) to e1.length by sep;
                 d2: BigDecimal <- BigDecimal(0) to e2.length by sep)
              yield PLArc(arc, d1, d2)
          case s2: PantsSeam =>
            for (d1: BigDecimal <- BigDecimal(0) to e1.length by sep;
                 d2: BigDecimal <- BigDecimal(0) to SkewPantsHexagon
                   .getSeamLength(arc.face, s2) by sep)
              yield PLArc(arc, d1, d2)
        }
      case s1: PantsSeam =>
        arc.terminalEdge match {
          case e2: SkewCurveEdge =>
            for {
              d1: BigDecimal <- BigDecimal(0) to SkewPantsHexagon.getSeamLength(
                arc.face,
                s1
              ) by sep
              d2: BigDecimal <- BigDecimal(0) to e2.length by sep
            } yield PLArc(arc, d1, d2)
          case s2: PantsSeam =>
            for {
              d1: BigDecimal <- BigDecimal(0) to SkewPantsHexagon.getSeamLength(
                arc.face,
                s1
              ) by sep
              d2: BigDecimal <- BigDecimal(0) to SkewPantsHexagon.getSeamLength(
                arc.face,
                s2
              ) by sep
            } yield PLArc(arc, d1, d2)
        }
    }
  }.to(ParSet)
  def freeEnumeratePath(
      arc: NormalArc[SkewPantsHexagon],
      sep: BigDecimal
  ): ParSet[PLPath] = {
    require(
      !(arc.terminalEdge.isInstanceOf[BoundaryEdge] || arc.initialEdge
        .isInstanceOf[BoundaryEdge])
    )
    arc.initialEdge match {
      case e1: SkewCurveEdge =>
        arc.terminalEdge match {
          case e2: SkewCurveEdge =>
            for {
              d1: BigDecimal <- BigDecimal(0) to e1.length by sep
              d2: BigDecimal <- BigDecimal(0) to e2.length by sep
            } yield
              PLPath(
                NormalPath[SkewPantsHexagon](Vector(arc)),
                Vector(d1),
                Vector(d2)
              )
          case s2: PantsSeam =>
            for {
              d1: BigDecimal <- BigDecimal(0) to e1.length by sep;
              d2: BigDecimal <- BigDecimal(0) to SkewPantsHexagon.getSeamLength(
                arc.face,
                s2
              ) by sep
            } yield
              PLPath(
                NormalPath[SkewPantsHexagon](Vector(arc)),
                Vector(d1),
                Vector(d2)
              )
        }
      case s1: PantsSeam =>
        arc.terminalEdge match {
          case e2: SkewCurveEdge =>
            for {
              d1: BigDecimal <- BigDecimal(0) to SkewPantsHexagon.getSeamLength(
                arc.face,
                s1
              ) by sep
              d2: BigDecimal <- BigDecimal(0) to e2.length by sep
            } yield
              PLPath(
                NormalPath[SkewPantsHexagon](Vector(arc)),
                Vector(d1),
                Vector(d2)
              )
          case s2: PantsSeam =>
            for {
              d1: BigDecimal <- BigDecimal(0) to SkewPantsHexagon.getSeamLength(
                arc.face,
                s1
              ) by sep
              d2: BigDecimal <- BigDecimal(0) to SkewPantsHexagon.getSeamLength(
                arc.face,
                s2
              ) by sep
            } yield
              PLPath(
                NormalPath[SkewPantsHexagon](Vector(arc)),
                Vector(d1),
                Vector(d2)
              )
        }
    }
  }.to(ParSet)

  def fixedInitialEnumerate(
      arc: NormalArc[SkewPantsHexagon],
      initialDisplacement: BigDecimal,
      sep: BigDecimal
  ): ParSet[PLArc] = {
    require(
      !(arc.terminalEdge.isInstanceOf[BoundaryEdge] || arc.initialEdge
        .isInstanceOf[BoundaryEdge])
    )
    arc.terminalEdge match {
      case e2: SkewCurveEdge =>
        for (d2: BigDecimal <- BigDecimal(0) to e2.length by sep)
          yield PLArc(arc, initialDisplacement, d2)
      case s2: PantsSeam =>
        for {
          d2: BigDecimal <- BigDecimal(0) to SkewPantsHexagon.getSeamLength(
            arc.face,
            s2
          ) by sep
        } yield PLArc(arc, initialDisplacement, d2)
    }
  }.to(ParSet)
}

case class PLPath(
    base: NormalPath[SkewPantsHexagon],
    initialDisplacements: Vector[BigDecimal],
    finalDisplacements: Vector[BigDecimal]
) {
  require(
    (base.edges.size == initialDisplacements.size) && (base.edges.size == finalDisplacements.size)
  )
  require(
    !(base.edges.head.initialEdge
      .isInstanceOf[BoundaryEdge] || base.edges.last.terminalEdge
      .isInstanceOf[BoundaryEdge])
  )
  val PLArcs: Vector[PLArc] = for (i <- (0 to base.edges.size - 1).toVector)
    yield PLArc(base.edges(i), initialDisplacements(i), finalDisplacements(i))
  require(
    PLArcs.zip(PLArcs.tail).forall {
      case (arc1, arc2) =>
        arc1.base.terminalEdge match {
          case s1: SkewCurveEdge =>
            arc2.base.initialEdge match {
              case s2: SkewCurveEdge =>
                SkewCurveEdge
                  .getPos(s1, arc1.finalDisplacement) == SkewCurveEdge
                  .getPos(s2, arc2.initialDisplacement)
              case p2: PantsSeam => false
            }
          case p1: PantsSeam =>
            arc2.base.initialEdge match {
              case s2: SkewCurveEdge => false
              case p2: PantsSeam =>
                PantsSeam.compareSeamPoints(
                  p1,
                  arc1.finalDisplacement,
                  p2,
                  arc2.initialDisplacement,
                  SkewPantsHexagon.getSeamLength(arc1.base.face, p1)
                )
            }
        }
    }
  )
  lazy val length: Double = PLArcs.map(arc => arc.length).sum
  def isClosed(tol: Double): Boolean = base.isClosed match {
    case false => false
    case true =>
      (base.initialEdge == base.terminalEdge) match {
        case true =>
          (math.abs(
            (initialDisplacements.head - finalDisplacements.last).toDouble
          ) < tol)
        case false =>
          base.terminalEdge match {
            case s: SkewCurveEdge =>
              (math.abs(
                (initialDisplacements.head - (s.length - finalDisplacements.last)).toDouble
              ) < tol)
            case p: PantsSeam =>
              (math.abs(
                (initialDisplacements.head - (SkewPantsHexagon.getSeamLength(
                  base.terminalFace,
                  p
                ) - finalDisplacements.last)).toDouble
              ) < tol)
          }
      }
  }
}

object PLPath {
  def findInitDisplacement(
      arc1: NormalArc[SkewPantsHexagon],
      arc1displacement: BigDecimal,
      arc2: NormalArc[SkewPantsHexagon]
  ): BigDecimal = {
    require(
      (arc1.terminalEdge == arc2.initialEdge) || (arc1.terminalEdge == arc2.initialEdge.flip)
    )
    require(!(arc1.terminalEdge.isInstanceOf[BoundaryEdge]))
    arc2.initialEdge match {
      case e2: SkewCurveEdge =>
        if (arc1.terminalEdge == arc2.initialEdge) arc1displacement
        else (e2.length - arc1displacement)
      case s2: PantsSeam =>
        if (arc1.terminalEdge == arc2.initialEdge) arc1displacement
        else (SkewPantsHexagon.getSeamLength(arc2.face, s2) - arc1displacement)
    }
  }

  def addPLArc(
      accum: ParSet[PLPath],
      baseedges: Vector[NormalArc[SkewPantsHexagon]],
      numdone: Index,
      sep: BigDecimal
  ): ParSet[PLPath] = {
    if (numdone == baseedges.size) accum
    else {
      baseedges(numdone).terminalEdge match {
        case e2: SkewCurveEdge =>
          for {
            path <- accum
            d2: BigDecimal <- (BigDecimal(0) to e2.length by sep)
          } yield
            PLPath(
              path.base.:+(baseedges(numdone)),
              path.initialDisplacements :+ findInitDisplacement(
                baseedges(numdone - 1),
                path.finalDisplacements.last,
                baseedges(numdone)
              ),
              path.finalDisplacements :+ d2
            )
        case s2: PantsSeam =>
          for {
            path <- accum
            d2: BigDecimal <- BigDecimal(0) to SkewPantsHexagon.getSeamLength(
              baseedges(numdone).face,
              s2
            ) by sep
          } yield
            PLPath(
              path.base.:+(baseedges(numdone)),
              path.initialDisplacements :+ findInitDisplacement(
                baseedges(numdone - 1),
                path.finalDisplacements.last,
                baseedges(numdone)
              ),
              path.finalDisplacements :+ d2
            )
      }
    }.to(ParSet)
  }

  def pickMinimal(paths: ParSet[PLPath], bound: Double): ParSet[PLPath] = {
    paths
      .groupBy(p => (p.initialDisplacements.head, p.finalDisplacements.last))
      .map {
        case (_, s) => s.minBy(_.length)
      }
      .to(ParSet)
      .filter(path => (path.length <= bound))
  }

  @annotation.tailrec
  def enumMinimalRec(
      accum: ParSet[PLPath],
      baseedges: Vector[NormalArc[SkewPantsHexagon]],
      numdone: Index,
      sep: BigDecimal,
      bound: Double
  ): ParSet[PLPath] = {
    if (numdone == baseedges.size) accum
    else
      enumMinimalRec(
        pickMinimal(addPLArc(accum, baseedges, numdone, sep), bound),
        baseedges,
        numdone + 1,
        sep,
        bound
      )
  }

  def enumMinimal(
      base: NormalPath[SkewPantsHexagon],
      sep: BigDecimal,
      bound: Double
  ): ParSet[PLPath] = {
    enumMinimalRec(
      PLArc
        .freeEnumeratePath(base.edges.head, sep)
        .filter(path => (path.length <= bound)),
      base.edges,
      1,
      sep,
      bound
    )
  }

  def enumMinimalClosed(
      base: NormalPath[SkewPantsHexagon],
      sep: BigDecimal,
      bound: Double
  ): Option[PLPath] = {
    enumMinimal(base, sep, bound).seq.minByOption(
      p =>
        math.abs(
          p.initialDisplacements.head.toDouble - findInitDisplacement(
            p.base.edges.last,
            p.finalDisplacements.last,
            p.base.edges.head
          ).toDouble
        )
    ) match {
      case None => None
      case Some(p) =>
        if (math.abs(
              p.initialDisplacements.head.toDouble - findInitDisplacement(
                p.base.edges.last,
                p.finalDisplacements.last,
                p.base.edges.head
              ).toDouble
            ) <= (2 * sep)) Some(p)
        else None
    }
  }

  def setEnumMinimalClosedRec(
      accum: Set[Option[PLPath]],
      paths: Set[NormalPath[SkewPantsHexagon]],
      sep: BigDecimal,
      bound: Double,
      tol: Double
  ): Set[Option[PLPath]] = {
    if (paths.isEmpty) accum
    else {
      val newplpath = enumMinimalClosed(paths.head, sep, bound)
      setEnumMinimalClosedRec(
        Set(newplpath),
        paths.tail,
        sep,
        newplpath.map(p => (p.length + tol)).getOrElse(bound).min(bound),
        tol
      )
    }
  }

  def setEnumMinimalClosed(
      paths: Set[NormalPath[SkewPantsHexagon]],
      sep: BigDecimal,
      bound: Double,
      tol: Double
  ): Set[Option[PLPath]] = {
    val newplpath = enumMinimalClosed(paths.head, sep, bound)
    setEnumMinimalClosedRec(
      Set(newplpath),
      paths.tail,
      sep,
      newplpath.map(p => (p.length + tol)).getOrElse(bound).min(bound),
      tol
    )
  }
}
