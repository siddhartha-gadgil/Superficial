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
          SkewPantsHexagon.displacementFromPantsBoundaryVertex(
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
          SkewPantsHexagon.displacementFromPantsBoundaryVertex(
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
          SkewPantsHexagon.skewIndexToHexagonIndex(base.face, base.initial),
          SkewPantsHexagon.skewIndexToHexagonIndex(base.face, base.terminal),
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
          SkewPantsHexagon.skewIndexToHexagonIndex(base.face, base.initial),
          SkewPantsHexagon.skewIndexToHexagonIndex(base.face, base.terminal),
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

  /**
    * Length 1 PL paths corresponding to a normal arc
    *
    * @param arc a given normal arc
    * @param sep separation of the endpoints
    * @return set of length-1 pl-paths
    */
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
              d1: BigDecimal <- BigDecimal(0) to e1.length by sep
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
  val plArcs: Vector[PLArc] = for (i <- (0 to (base.edges.size - 1)).toVector)
    yield PLArc(base.edges(i), initialDisplacements(i), finalDisplacements(i))
  require(
    plArcs.zip(plArcs.tail).forall {
      case (arc1, arc2) =>
        arc1.base.terminalEdge match {
          case s1: SkewCurveEdge =>
            arc2.base.initialEdge match {
              case s2: SkewCurveEdge =>
                SkewCurveEdge.comparePoints(
                  s1,
                  arc1.finalDisplacement,
                  s2,
                  arc2.initialDisplacement
                )
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
  lazy val length: Double = plArcs.map(arc => arc.length).sum
  def isClosed(tol: Double): Boolean = base.isClosed && {
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

  /**
    * determine initial displacement of the second arc to glue with first arc
    *
    * @param arc1 first normal arc (not pl-arc)
    * @param arc1displacement final displacement of first arc
    * @param arc2 second normal arc (not pl-arc)
    * @return initial displacement of second arc
    */
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

  def appendPLArc(
      accum: ParSet[PLPath],
      baseEdges: Vector[NormalArc[SkewPantsHexagon]],
      numdone: Index,
      sep: BigDecimal
  ): ParSet[PLPath] = {
    if (numdone == baseEdges.size) accum
    else {
      baseEdges(numdone).terminalEdge match {
        case e2: SkewCurveEdge =>
          for {
            path <- accum
            d2: BigDecimal <- (BigDecimal(0) to e2.length by sep)
          } yield
            PLPath(
              path.base.:+(baseEdges(numdone)),
              path.initialDisplacements :+ findInitDisplacement(
                baseEdges(numdone - 1),
                path.finalDisplacements.last,
                baseEdges(numdone)
              ),
              path.finalDisplacements :+ d2
            )
        case s2: PantsSeam =>
          for {
            path <- accum
            d2: BigDecimal <- BigDecimal(0) to SkewPantsHexagon.getSeamLength(
              baseEdges(numdone).face,
              s2
            ) by sep
          } yield
            PLPath(
              path.base.:+(baseEdges(numdone)),
              path.initialDisplacements :+ findInitDisplacement(
                baseEdges(numdone - 1),
                path.finalDisplacements.last,
                baseEdges(numdone)
              ),
              path.finalDisplacements :+ d2
            )
      }
    }.to(ParSet)
  }

  def pickMinimal(paths: ParSet[PLPath], bound: Double): ParSet[PLPath] = {
    paths
      .filter(path => (path.length <= bound))
      .groupBy(p => (p.initialDisplacements.head, p.finalDisplacements.last))
      .map {
        case (_, s) => s.minBy(_.length)
      }
      .to(ParSet)
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
        pickMinimal(appendPLArc(accum, baseedges, numdone, sep), bound),
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
    enumMinimal(base, sep, bound).groupBy(_.initialDisplacements.head).map {
      case (_, s) => s.minBy(p =>
        math.abs(
          p.initialDisplacements.head.toDouble - findInitDisplacement(
            p.base.edges.last,
            p.finalDisplacements.last,
            p.base.edges.head
          ).toDouble
        ))
    }.filter { p =>
      math.abs(
        p.initialDisplacements.head.toDouble - findInitDisplacement(
          p.base.edges.last,
          p.finalDisplacements.last,
          p.base.edges.head
        ).toDouble
      ) <= (2 * sep)
    }.seq.minByOption(_.length)
  }



  /**
    * shorten a pl-path by crossing a vertex replacing three arcs with the middle arc short by two arcs
    *
    * @param complex the surface
    * @param path the original path
    * @param sep separation
    * @return shortened pl-arc
    */
  def shortenPathCrossingVertex(
      complex: TwoComplex[SkewPantsHexagon],
      path: PLPath,
      sep: BigDecimal
  ): NormalPath[SkewPantsHexagon] = {
    require(path.plArcs.map(_.length).min < sep * 3)
    val shortestedgeindex = path.plArcs.indexOf(path.plArcs.minBy(_.length))
    val shortestedge = path.base.edges(shortestedgeindex)
    if (shortestedge.whichVertexLinking.get == shortestedge.initialEdge.terminal)
      surgery(complex, path.base, shortestedgeindex, -1)
    else surgery(complex, path.base, shortestedgeindex, 1)
  }

  // Helper for shortening path by crossing a vertex
  def surgery(
      complex: TwoComplex[SkewPantsHexagon],
      path: NormalPath[SkewPantsHexagon],
      shortestedgeindex: Index,
      arc1edgeshift: Int
  ): NormalPath[SkewPantsHexagon] = {
    require(math.abs(arc1edgeshift) == 1, "newarcedgeshift must be 1 or -1")
    if (shortestedgeindex == 0) {
      NormalPath(
        replacingArcs(
          complex,
          path.edges.last,
          path.edges(0),
          path.edges(1),
          arc1edgeshift
        ) ++ path.edges.drop(2).dropRight(1)
      )
    } else if (shortestedgeindex == (path.length - 1)) {
      NormalPath(
        path.edges.slice(0, shortestedgeindex - 1).drop(1) ++ replacingArcs(
          complex,
          path.edges(shortestedgeindex - 1),
          path.edges(shortestedgeindex),
          path.edges(0),
          arc1edgeshift
        )
      )
    } else {
      NormalPath(
        path.edges.slice(0, shortestedgeindex - 1) ++ replacingArcs(
          complex,
          path.edges(shortestedgeindex - 1),
          path.edges(shortestedgeindex),
          path.edges(shortestedgeindex + 1),
          arc1edgeshift
        ) ++ path.edges.drop(shortestedgeindex + 1)
      )
    }
  }

  // helper for surgery
  def replacingArcs(
      complex: TwoComplex[SkewPantsHexagon],
      arc1: NormalArc[SkewPantsHexagon],
      arc2: NormalArc[SkewPantsHexagon],
      arc3: NormalArc[SkewPantsHexagon],
      arc1edgeshift: Int
  ): Vector[NormalArc[SkewPantsHexagon]] = {
    val newarc1 = NormalArc(
      arc1.initial,
      (arc1.terminal + arc1edgeshift) % arc1.face.sides,
      arc1.face
    )
    val newarc2faceandinit = (complex
      .edgeIndices(newarc1.terminalEdge)
      .map(p => (p._1, p._2)) - (newarc1.face -> newarc1.terminal)).head
    require(
      newarc2faceandinit._1 == arc3.face,
      "Suggested arc cannot be defined"
    )
    Vector(newarc1, NormalArc(newarc2faceandinit._2, arc3.terminal, arc3.face))
  }

  def skewlessShortenPathCrossingVertex(
      complex: TwoComplex[SkewPantsHexagon],
      path: PLPath,
      sep: BigDecimal
  ): NormalPath[SkewPantsHexagon] = {
    require(path.plArcs.map(_.length).min < sep * 3)
    val shortestedgeindex = path.plArcs.indexOf(path.plArcs.minBy(_.length))
    val shortestedge = path.base.edges(shortestedgeindex)
    if (shortestedge.whichVertexLinking.get == shortestedge.initialEdge.terminal)
      skewlessSurgery(complex, path.base, shortestedgeindex, -1)
    else skewlessSurgery(complex, path.base, shortestedgeindex, 1)
  }

  def skewlessSurgery(
      complex: TwoComplex[SkewPantsHexagon],
      path: NormalPath[SkewPantsHexagon],
      shortestedgeindex: Index,
      arc1edgeshift: Int
  ): NormalPath[SkewPantsHexagon] = {
    require(math.abs(arc1edgeshift) == 1, "newarcedgeshift must be 1 or -1")
    if (shortestedgeindex == 0) {
      NormalPath(
        skewlessReplacingArcs(
          complex,
          path.edges.last,
          path.edges(0),
          path.edges(1),
          arc1edgeshift
        ) ++ path.edges.drop(2).dropRight(1)
      )
    } else if (shortestedgeindex == (path.length - 1)) {
      NormalPath(
        path.edges
          .slice(0, shortestedgeindex - 1)
          .drop(1) ++ skewlessReplacingArcs(
          complex,
          path.edges(shortestedgeindex - 1),
          path.edges(shortestedgeindex),
          path.edges(0),
          arc1edgeshift
        )
      )
    } else {
      NormalPath(
        path.edges.slice(0, shortestedgeindex - 1) ++ skewlessReplacingArcs(
          complex,
          path.edges(shortestedgeindex - 1),
          path.edges(shortestedgeindex),
          path.edges(shortestedgeindex + 1),
          arc1edgeshift
        ) ++ path.edges.drop(shortestedgeindex + 1)
      )
    }
  }

  def skewlessReplacingArcs(
      complex: TwoComplex[SkewPantsHexagon],
      arc1: NormalArc[SkewPantsHexagon],
      arc2: NormalArc[SkewPantsHexagon],
      arc3: NormalArc[SkewPantsHexagon],
      arc1edgeshift: Int
  ): Vector[NormalArc[SkewPantsHexagon]] = {
    val newarc1 = NormalArc(
      arc1.initial,
      (arc1.terminal + arc1edgeshift) % arc1.face.sides,
      arc1.face
    )
    val newarc2faceandinit = (complex
      .edgeIndices(newarc1.terminalEdge)
      .map(p => (p._1, p._2)) - (newarc1.face -> newarc1.terminal)).head
    val newarc2 = NormalArc(
      newarc2faceandinit._2,
      newarc2faceandinit._2 - arc1edgeshift,
      newarc2faceandinit._1
    )
    val newarc3faceandinit = (complex
      .edgeIndices(newarc2.terminalEdge)
      .map(p => (p._1, p._2)) - (newarc2.face -> newarc2.terminal)).head
    require(
      newarc3faceandinit._1 == arc3.face,
      "Suggested arc cannot be defined"
    )
    Vector(
      newarc1,
      newarc2,
      NormalArc(newarc3faceandinit._2, arc3.terminal, arc3.face)
    )
  }

  def isotopicNearby(
      complex: TwoComplex[SkewPantsHexagon],
      baseplpath: PLPath,
      paths: Set[PLPath]
  ): Set[PLPath] = {
    val nearbyarcs = NormalPath.pathNeighbouringArcs(complex, baseplpath.base)
    paths.filter(p => p.base.edges.toSet.subsetOf(nearbyarcs))
  }
}
