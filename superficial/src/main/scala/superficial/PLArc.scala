package superficial
import scala.collection.parallel._, immutable.ParSet
import upickle.default.{ReadWriter => RW, macroRW, _}

import Polygon.Index
import scala.tools.nsc.doc.html.HtmlTags
import doodle.syntax.path

case class PLArc(
    base: NormalArc[SkewPantsHexagon],
    initialDisplacement: BigDecimal,
    finalDisplacement: BigDecimal
) {
  require(
    !(base.face.boundary(base.initial).isInstanceOf[BoundaryEdge] || base.face
      .boundary(base.terminal)
      .isInstanceOf[BoundaryEdge])
  )
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

  /**
    * Checks whether the final point of the PLArc is close to any vertex in its terminalEdge
    * Optionally returns true if close to initialvertex of terminaledge and false if close to terminalalvertex of terminaledge
    * Returns None if the final point is not close to any vertex
    *
    * @param threshold how close the final point should be to a vertex
    * @return
    */
  def finalPointClosetoVertex(threshold: BigDecimal): Option[Boolean] = {
    if (finalDisplacement < threshold) Some(true)
    else if (math.abs(
               base.face
                 .sideLength(base.terminalEdge) - finalDisplacement.toDouble
             ) < threshold) Some(false)
    else None
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
  implicit val rw: RW[PLPath] = macroRW

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

  /**
    * Append a PLArc to a PLPath
    *
    * @param accum PLPath to which a PLArc has to be appended from an underlying normalpath
    * @param baseEdges Edges of a NormalPath part of which has been enumerated into a PLPath
    * @param numdone Number of edges of the NormalPath that have been enumerated into accum
    * @param sep Separation of endpoints of the appended PLArc
    * @return Set of PLPaths extending accum by one PLArcs with the endpoints of PLPaths separated by sep
    */
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

  /**
    * From a set of PLPaths with the same base, return a set of PLPaths that contains only the shortest path for each choice of start and end point
    * and only if the shortest path has length less than a specified bound
    *
    * @param paths set of paths
    * @param bound length bound
    * @return
    */
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

  /**
    * For a given NormalPath, enumerate minimal PLPaths with endpoints separated by sep and length less than bound
    *
    * @param base NormalPath to be enumerated
    * @param sep separation between endpoints
    * @param bound length bound
    * @return Set of PLPaths
    */
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

  /**
    * Given a closed NormalPath, optionally return the minimal PLPath corresponding to it if its length is less than a given bound
    *
    * @param base the NormalPath
    * @param sep separation between endpoints while enumerating PLPaths
    * @param bound length bound
    * @return
    */
  def enumMinimalClosed(
      base: NormalPath[SkewPantsHexagon],
      sep: BigDecimal,
      bound: Double
  ): Option[PLPath] = {
    require(base.isClosed, s"$base is not closed")
    enumMinimal(base, sep, bound)
      .groupBy(_.initialDisplacements.head)
      .map {
        case (_, s) =>
          s.minBy(
            p =>
              math.abs(
                p.initialDisplacements.head.toDouble - findInitDisplacement(
                  p.base.edges.last,
                  p.finalDisplacements.last,
                  p.base.edges.head
                ).toDouble
              )
          )
      }
      .filter { p =>
        math.abs(
          p.initialDisplacements.head.toDouble - findInitDisplacement(
            p.base.edges.last,
            p.finalDisplacements.last,
            p.base.edges.head
          ).toDouble
        ) <= (2 * sep)
      }
      .seq
      .minByOption(_.length)
  }

  /**
    * Optional map from closed NormalPaths to their minimal PL representative
    * where a NormalPath maps to None if the minimal PLPath has length greater than specified bound
    *
    * @param paths the set of paths
    * @param sep separation between endpoints while enumerating candidates for minimal PLPath
    * @param bound the length bound
    * @return
    */
  def enumMinimalClosedFamily(
      paths: Vector[NormalPath[SkewPantsHexagon]],
      sep: BigDecimal,
      bound: Double
  ): Map[NormalPath[SkewPantsHexagon], Option[PLPath]] = {
    require(paths.forall(_.isClosed), "All paths are not closed")
    paths.zip(paths.map(p => enumMinimalClosed(p, sep, bound))).toMap
  }

  /**
    * Make enumerated PL paths into strings to save
    *
    * @param data the enumerated PL paths
    * @return Json encoded data
    */
  def minimalClosedFamilyToJson(
      data: Map[NormalPath[SkewPantsHexagon], Option[PLPath]]
  ): String = write(data)

  /**
    * Read enumerated PL paths from string
    *
    * @param js string with json data
    * @return enumerated shortest paths
    */
  def minimalClosedFamilyFromJson(js: String) =
    read[Map[NormalPath[SkewPantsHexagon], Option[PLPath]]](js)

  /**
    * Shorten a PL-path by moving path across vertices and checking if length reduces
    *
    * @param complex the surface
    * @param path the PLPath
    * @param sep separation between endpoints while enumerating candidates for minimal PLPath
    * @param uniqrepuptoflipandcyclicper map from normalpath to the candidate which is enumerated to PLPath
    * @param enumdata map from the normalpaths that are enumerated to their minimal PL representatives
    * @return Shortened PLPath
    */
  def shorten(
      complex: TwoComplex[SkewPantsHexagon],
      path: PLPath,
      sep: BigDecimal,
      uniqrepuptoflipandcyclicper: Map[NormalPath[SkewPantsHexagon], NormalPath[
        SkewPantsHexagon
      ]],
      enumdata: Map[NormalPath[SkewPantsHexagon], Option[PLPath]]
  ): Option[PLPath] = {
    require(path.base.isClosed, "Path is not closed")
    val arcsclosetovertex: Vector[(Boolean, Int)] = path.plArcs
      .map(_.finalPointClosetoVertex(1.5 * sep))
      .zipWithIndex
      .flatMap { case (optB, j) => optB.map(b => (b, j)) }
    if (path.base.isVertexLinking) None
    else if (arcsclosetovertex.nonEmpty) {
      // all paths obtained by pushing across a vertex (vertex arbitrarily chosen)
      val normalpaths = (for { i <- arcsclosetovertex } yield
        NormalPath.otherWayAroundVertex(complex, path.base, i._2, i._1))
      if (normalpaths.contains(None)) None
      else {
        val plpaths = for {
          path <- normalpaths.flatten
          shortPathOpt = uniqrepuptoflipandcyclicper.get(path)
          _ = assert(
            shortPathOpt.nonEmpty,
            s"path $path not in uniqueUptoFlipAndCyclicPer"
          )
          shortPath <- shortPathOpt
          spOpt = enumdata.get(shortPath)
          _ = assert(spOpt.nonEmpty, s"path $shortPath not in enumdata")
          sp <- spOpt
        } yield sp

        val newminplpathOpt = plpaths.flatten
          .zip(plpaths.flatten.map(_.length))
          .minByOption(_._2)
        newminplpathOpt
          .map { newminplpath =>
            if (newminplpath._2 < path.length)
              shorten(
                complex,
                newminplpath._1,
                sep,
                uniqrepuptoflipandcyclicper,
                enumdata
              )
            else Some(path)
          }
          .getOrElse(Some(path))
      }
    } else Some(path)
  }

  /**
    * Remove duplicates upto flips and cyclic permutations given a set of closed PLPaths
    *
    * @param paths the set of plpaths
    * @param accum parameter used for recursion
    * @return Set of PLPaths with duplicates removed
    */
  def removeFlipAndCyclicPer(
      paths: Set[PLPath],
      accum: Set[PLPath] = Set()
  ): Set[PLPath] = {
    if (paths.isEmpty) accum
    else {
      removeFlipAndCyclicPer(
        paths.tail.filter(
          p =>
            (p.base.edges.size != paths.head.base.edges.size) || ((p.base.edges.toSet != paths.head.base.edges.toSet) && (p.base.edges.toSet != paths.head.base.flip.edges.toSet))
        ),
        accum + paths.head
      )
    }
  }

  /**
    * Removes paths from the first set that are present in the second set
    * upto flips and cyclic permutations
    *
    * @param paths set of paths from which to be removed
    * @param toremove set of paths which are to be removed
    * @return Set obtained after removing paths
    */
  def removeGivenPaths(
      paths: Set[PLPath],
      toremove: Set[PLPath]
  ): Set[PLPath] = {
    val toremovevec = toremove.toVector
    val removedata = toremovevec
      .map(_.base.length)
      .zip(toremovevec.map(_.base.edges.toSet)) ++ toremovevec
      .map(_.base.length)
      .zip(toremovevec.map(_.base.edges.map(_.flip).toSet))
    paths.filter(
      p => !(removedata.contains((p.base.length, p.base.edges.toSet)))
    )
  }

  /**
    * Gives isotopy classes of a given set of closed PLPaths where the isotopy classes are generated by
    * only those isotopies which increase length slightly.
    * Isotopy classes may have paths not in the original set of paths.
    *
    * @param complex the surface
    * @param paths the set of paths
    * @param sep separation between endpoints while enumerating candidates for minimal PLPath
    * @param uniqrepuptoflipandcyclicper map from normalpath to the candidate which is enumerated to PLPath
    * @param enumdata  map from the normalpaths that are enumerated to their minimal PL representatives
    * @param accum parameter for recursion
    * @return Set of isotopy classes
    */
  def postEnumIsotopyCheck(
      complex: TwoComplex[SkewPantsHexagon],
      paths: Set[PLPath],
      sep: BigDecimal,
      uniqrepuptoflipandcyclicper: Map[NormalPath[SkewPantsHexagon], NormalPath[
        SkewPantsHexagon
      ]],
      enumdata: Map[NormalPath[SkewPantsHexagon], Option[PLPath]],
      accum: Set[Set[PLPath]] = Set()
  ): Set[Set[PLPath]] = {
    require(paths.map(_.base).forall(_.isClosed), "All paths are not closed")
    if (paths.isEmpty) accum
    else {
      val eqclass = getPostEnumIsotopyClass(
        complex,
        paths.head,
        sep,
        uniqrepuptoflipandcyclicper,
        enumdata
      )
      postEnumIsotopyCheck(
        complex,
        removeGivenPaths(paths, eqclass),
        sep,
        uniqrepuptoflipandcyclicper,
        enumdata,
        accum + eqclass
      )
    }
  }

  /** Return the isotopy class of a closed PLPath generated by
    * only those isotopies which increase the length slightly.
    *
    * @param complex the surface
    * @param path the PLPath
    * @param sep separation between endpoints while enumerating candidates for minimal PLPath
    * @param uniqrepuptoflipandcyclicper map from normalpath to the candidate which is enumerated to PLPath
    * @param enumdata map from the normalpaths that are enumerated to their minimal PL representatives
    * @param accum parameter for recursion
    * @return The isotopy class
    */
  def getPostEnumIsotopyClass(
      complex: TwoComplex[SkewPantsHexagon],
      path: PLPath,
      sep: BigDecimal,
      uniqrepuptoflipandcyclicper: Map[NormalPath[SkewPantsHexagon], NormalPath[
        SkewPantsHexagon
      ]],
      enumdata: Map[NormalPath[SkewPantsHexagon], Option[PLPath]],
      accum: Set[PLPath] = Set()
  ): Set[PLPath] = {
    val arcsclosetovertex = path.plArcs
      .map(_.finalPointClosetoVertex(1.5 * sep))
      .zipWithIndex
      .filter(_._1.isDefined)
    if (arcsclosetovertex.nonEmpty) {
      val normalpaths = (for { i <- arcsclosetovertex } yield
        NormalPath.otherWayAroundVertex(complex, path.base, i._2, i._1.get))
      val plpaths = (for {
        path <- normalpaths.flatten
        repPath <- uniqrepuptoflipandcyclicper.get(path)
      } yield
        enumdata
          .get(
            repPath
          )
          .get).flatten.filter(_.length < (path.length + 3 * sep)).toSet
      (for { plpath <- plpaths.diff(accum) } yield
        getPostEnumIsotopyClass(
          complex,
          plpath,
          sep,
          uniqrepuptoflipandcyclicper,
          enumdata,
          accum + path
        )).foldLeft(accum + path)((a, b) => a.union(b))
    } else Set(path)
  }

  /**
    * Given a surface, gives PLPaths that are shortest in their isotopy class
    * and have length not much more than the smallest pants cuff
    *
    * @param surf the surface
    * @param sizebound size of the NormalPaths to be enumerated
    * @param sep separation between endpoints while enumerating candidates for minimal PLPath
    * @param tol multiplier of length of smallest pants cuff giving the length bound for PLPaths (ex - 1.3)
    * @return Set of PLPaths
    */
  def shortPathsfromSurface(
      surf: SkewPantsSurface,
      sizebound: Int,
      sep: BigDecimal,
      tol: BigDecimal
  ): Set[PLPath] = {
    require(surf.isClosedSurface, "Surface is not closed")
    val enumlenbound: Double = ((surf.cs.map(_.length).min) * tol).toDouble
    val uniqclpaths
        : Map[NormalPath[SkewPantsHexagon], NormalPath[SkewPantsHexagon]] =
      NormalPath.uniqueUptoFlipAndCyclicPerm(
        NormalPath
          .enumerate[SkewPantsHexagon](surf, Some(sizebound))
          .filter(_.isClosed)
      )
    val plpaths: Map[NormalPath[SkewPantsHexagon], Option[PLPath]] =
      PLPath.enumMinimalClosedFamily(
        uniqclpaths.values.toVector,
        sep,
        enumlenbound
      )
    val ndgplpaths: Vector[PLPath] =
      plpaths.values.flatten.toVector.filter(_.base.length < sizebound)
    val shortPaths: Set[PLPath] = PLPath.removeFlipAndCyclicPer(
      ndgplpaths
        .flatMap(path => PLPath.shorten(surf, path, sep, uniqclpaths, plpaths))
        .toSet
    )
    postEnumIsotopyCheck(surf, shortPaths, sep, uniqclpaths, plpaths).map(
      _.minBy(_.length)
    )
  }

  //Unsure of the mathematics behind this, pending
  def isotopicNearby(
      complex: TwoComplex[SkewPantsHexagon],
      baseplpath: PLPath,
      paths: Set[PLPath]
  ): Set[PLPath] = {
    val nearbyarcs = NormalPath.pathNeighbouringArcs(complex, baseplpath.base)
    paths.filter(p => p.base.edges.toSet.subsetOf(nearbyarcs))
  }

  def shortPathsData(
      surf: SkewPantsSurface,
      sizebound: Int,
      sep: BigDecimal,
      tol: BigDecimal
  ) = {
    require(surf.isClosedSurface, "Surface is not closed")
    val enumlenbound: Double = ((surf.cs.map(_.length).min) * tol).toDouble
    val clPaths = NormalPath
      .enumerate[SkewPantsHexagon](surf, Some(sizebound))
      .filter(_.isClosed)
    val uniqclpaths
        : Map[NormalPath[SkewPantsHexagon], NormalPath[SkewPantsHexagon]] =
      NormalPath.uniqueUptoFlipAndCyclicPerm(
        clPaths
      )
    val plpaths: Map[NormalPath[SkewPantsHexagon], Option[PLPath]] =
      PLPath.enumMinimalClosedFamily(
        uniqclpaths.values.toVector,
        sep,
        enumlenbound
      )
    val ndgplpaths: Vector[PLPath] =
      plpaths.values.flatten.toVector.filter(_.base.length < sizebound)
    val shortPaths: Set[PLPath] = PLPath.removeFlipAndCyclicPer(
      ndgplpaths
        .flatMap(path => PLPath.shorten(surf, path, sep, uniqclpaths, plpaths))
        .toSet
    )
    postEnumIsotopyCheck(surf, shortPaths, sep, uniqclpaths, plpaths).map(
      _.minBy(_.length)
    )
  }
}

case class ShortPathsData(
    surf: SkewPantsSurface,
    sizebound: Int,
    sep: BigDecimal,
    tol: BigDecimal,
    enumLenBound: Double,
    clPaths: Map[NormalPath[SkewPantsHexagon], NormalPath[SkewPantsHexagon]],
    uniqueClPaths: Map[NormalPath[SkewPantsHexagon], NormalPath[
      SkewPantsHexagon
    ]],
    plPaths: Map[NormalPath[SkewPantsHexagon], Option[PLPath]],
    baseShortPaths: Set[PLPath],
    shortPaths: Set[PLPath]
) {
  def toJson: String = write(this)
}

object ShortPathsData {
  implicit val rw: RW[ShortPathsData] = macroRW

  def fromJson(s: String) = read[ShortPathsData](s)
}
