package superficial
import scala.collection.parallel._, immutable.ParSet
import upickle.default.{ReadWriter => RW, macroRW, _}

import Polygon.Index
import scala.tools.nsc.doc.html.HtmlTags
import doodle.syntax.path

case class PLArc(
    base: NormalArc[SkewPantsHexagon],
    initialDisplacement: Double,
    finalDisplacement: Double
) {
  require(
    !(base.face.boundary(base.initial).isInstanceOf[BoundaryEdge] || base.face
      .boundary(base.terminal)
      .isInstanceOf[BoundaryEdge])
  )
  lazy val hexagonInitialDisplacement: Option[Double] =
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
  lazy val hexagonFinalDisplacement: Option[Double] =
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

  lazy val length: Double = {
    require(
      base.face.edgeLengths
        .forall(x => x.isDefined) && hexagonInitialDisplacement.isDefined && hexagonFinalDisplacement.isDefined
    )
    if (base.face.top) {
      Hexagon
      base.face.topHex
        .arcLength(
          SkewPantsHexagon.skewIndexToHexagonIndex(base.face, base.initial),
          SkewPantsHexagon.skewIndexToHexagonIndex(base.face, base.terminal),
          hexagonInitialDisplacement.get,
          hexagonFinalDisplacement.get
        )
    } else {
      base.face.bottomHex
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
  def finalPointClosetoVertex(threshold: Double): Option[Boolean] = {
    if (finalDisplacement < threshold) Some(true)
    else if (math.abs(
               base.face
                 .sideLength(base.terminalEdge) - finalDisplacement.toDouble
             ) < threshold) Some(false)
    else None
  }
}

object PLArc {
  def rng(init: Double)(term: Double)(sep: Double) =
    Vector.tabulate(((term - init) / sep).floor.toInt + 1)(j => init + j * sep)

  rng(3)(2)(1)

  def freeEnumerate(
      arc: NormalArc[SkewPantsHexagon],
      sep: Double
  ): ParSet[PLArc] = {
    require(
      !(arc.terminalEdge.isInstanceOf[BoundaryEdge] || arc.initialEdge
        .isInstanceOf[BoundaryEdge])
    )
    for (d1: Double <- rng(0)(arc.face.sideLength(arc.initialEdge))(sep);
         d2: Double <- rng(0)(arc.face.sideLength(arc.terminalEdge))(sep))
      yield PLArc(arc, d1, d2)
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
      sep: Double
  ): ParSet[PLPath] = {
    require(
      !(arc.terminalEdge.isInstanceOf[BoundaryEdge] || arc.initialEdge
        .isInstanceOf[BoundaryEdge])
    )
    for (d1: Double <- rng(0)(arc.face.sideLength(arc.initialEdge))(sep);
         d2: Double <- rng(0)(arc.face.sideLength(arc.terminalEdge))(sep))
      yield
        PLPath(
          NormalPath[SkewPantsHexagon](Vector(arc)),
          Vector(d1),
          Vector(d2)
        )
  }.to(ParSet)

  def fixedInitialEnumerate(
      arc: NormalArc[SkewPantsHexagon],
      initialDisplacement: Double,
      sep: Double
  ): ParSet[PLArc] = {
    require(
      !(arc.terminalEdge.isInstanceOf[BoundaryEdge] || arc.initialEdge
        .isInstanceOf[BoundaryEdge])
    )
    for (d2: Double <- rng(0)(arc.face.sideLength(arc.terminalEdge))(sep))
      yield PLArc(arc, initialDisplacement, d2)
  }.to(ParSet)
}

case class PLPath(
    base: NormalPath[SkewPantsHexagon],
    initialDisplacements: Vector[Double],
    finalDisplacements: Vector[Double]
) {
  require(
    (base.edges.size == initialDisplacements.size) && (base.edges.size == finalDisplacements.size)
  )
  require(
    !(base.edges.head.initialEdge
      .isInstanceOf[BoundaryEdge] || base.edges.last.terminalEdge
      .isInstanceOf[BoundaryEdge])
  )
  lazy val plArcs: Vector[PLArc] =
    for (i <- (0 to (base.edges.size - 1)).toVector)
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
        (math.abs(
          initialDisplacements.head - (base.terminalFace
            .sideLength(base.terminalEdge) - finalDisplacements.last)
        ) < tol)
    }
  }
}

object PLPath {
  import PLArc.rng
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
      arc1displacement: Double,
      arc2: NormalArc[SkewPantsHexagon]
  ): Double = {
    require(
      (arc1.terminalEdge == arc2.initialEdge) || (arc1.terminalEdge == arc2.initialEdge.flip)
    )
    require(!(arc1.terminalEdge.isInstanceOf[BoundaryEdge]))
    if (arc1.terminalEdge == arc2.initialEdge) arc1displacement
    else (arc2.face.sideLength(arc2.initialEdge) - arc1displacement)
  }

  def findFinalDisplacement(
      arc1: NormalArc[SkewPantsHexagon],
      arc2displacement: Double,
      arc2: NormalArc[SkewPantsHexagon]
  ): Double = {
    require(
      (arc1.terminalEdge == arc2.initialEdge) || (arc1.terminalEdge == arc2.initialEdge.flip)
    )
    require(!(arc1.terminalEdge.isInstanceOf[BoundaryEdge]))
    if (arc1.terminalEdge == arc2.initialEdge) arc2displacement
    else (arc1.face.sideLength(arc1.terminalEdge) - arc2displacement)
  }

  /**
    * Append a PLArc to a PLPath
    *
    * @param accum PLPath to which a PLArc has to be appended from an underlying normalpath
    * @param baseEdges Edges of a NormalPath part of which has been enumerated into a PLPath
    * @param numdone Number of edges of the NormalPath that have been enumerated into accum
    * @param sep Separation of endpoints of the appended PLArc
    * @return Set of PLPaths extending accum by one PLArcs with the endpoints of PLPaths separated.toDouble) (sep)
    */
  def appendPLArc(
      accum: ParSet[PLPath],
      baseEdges: Vector[NormalArc[SkewPantsHexagon]],
      numdone: Index,
      sep: Double
  ): ParSet[PLPath] = {
    if (numdone == baseEdges.size) accum
    else {
      for {
        path <- accum
        d2: Double <- (rng(0)(
          baseEdges(numdone).face.sideLength(baseEdges(numdone).terminalEdge)
        )(sep))
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
      sep: Double,
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
    * For a given NormalPath, enumerate minimal PLPaths with endpoints separated.toDouble) (sep) and length less than bound
    *
    * @param base NormalPath to be enumerated
    * @param sep separation between endpoints
    * @param bound length bound
    * @return Set of PLPaths
    */
  def enumMinimal(
      base: NormalPath[SkewPantsHexagon],
      sep: Double,
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

  def returnExactlyClosed(
      path: PLPath,
      threshold: Double
  ): Option[PLPath] = {
    require(path.base.isClosed, s"$path is not closed")
    if ((math.abs(
          findInitDisplacement(
            path.base.edges.last,
            path.plArcs.last.finalDisplacement,
            path.base.edges.head
          ) - path.plArcs.head.initialDisplacement
        )) < threshold) {
      Some(
        PLPath(
          path.base,
          path.initialDisplacements,
          path.finalDisplacements.dropRight(1) :+ findFinalDisplacement(
            path.base.edges.last,
            path.initialDisplacements.head,
            path.base.edges.head
          )
        )
      )
    } else None
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
      sep: Double,
      bound: Double
  ): Option[PLPath] = {
    require(base.isClosed, s"$base is not closed")
    enumMinimal(base, sep, bound)
      .map(path => returnExactlyClosed(path, 1.5 * sep))
      .flatten
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
      sep: Double,
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
    * @param shortestPlReps map from the normalpaths that are enumerated to their minimal PL representatives
    * @return Shortened PLPath
    */
  def shorten(
      complex: TwoComplex[SkewPantsHexagon],
      path: PLPath,
      sep: Double,
      uniqrepuptoflipandcyclicper: Map[NormalPath[SkewPantsHexagon], NormalPath[
        SkewPantsHexagon
      ]],
      shortestPlReps: Map[NormalPath[SkewPantsHexagon], Option[PLPath]]
  ): Option[PLPath] = {
    require(path.base.isClosed, "Path is not closed")
    val arcsclosetovertex: Vector[(Boolean, Int)] = path.plArcs
      .map(_.finalPointClosetoVertex(1.5 * sep))
      .zipWithIndex
      .flatMap { case (optB, j) => optB.map(b => (b, j)) }
    if (path.base.isVertexLinking) None
    else if (arcsclosetovertex.nonEmpty) {
      // all paths obtained by pushing across a vertex
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
          spOpt = shortestPlReps.get(shortPath)
          _ = assert(spOpt.nonEmpty, s"path $shortPath not in shortestPlReps")
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
                shortestPlReps
              )
            else Some(path)
          }
          .getOrElse(Some(path))
      }
    } else Some(path)
  }

  def shortenPf(
      complex: TwoComplex[SkewPantsHexagon],
      path: PLPath,
      sep: Double,
      uniqrepuptoflipandcyclicper: Map[NormalPath[SkewPantsHexagon], NormalPath[
        SkewPantsHexagon
      ]],
      shortestPlReps: Map[NormalPath[SkewPantsHexagon], Option[PLPath]],
      accumPf: FreeHomotopy[SkewPantsHexagon]
  ): (Option[PLPath], FreeHomotopy[SkewPantsHexagon]) = {
    require(path.base.isClosed, "Path is not closed")
    val arcsclosetovertex: Vector[(Boolean, Int)] = path.plArcs
      .map(_.finalPointClosetoVertex(1.5 * sep))
      .zipWithIndex
      .flatMap { case (optB, j) => optB.map(b => (b, j)) }
    if (path.base.isVertexLinking)
      None ->
        (accumPf | PathHomotopy
          .VertexLinking(path.base, path.base.edges.head.whichVertexLinking.get)
          .free) // homotopically trivial
    else if (arcsclosetovertex.isEmpty)
      Some(path) -> accumPf // no perturbations
    else {
      // all paths obtained by pushing across a vertex
      val normalpaths =
        for {
          i <- arcsclosetovertex
          (newPath, otherWayPf) = NormalPath.otherWayAroundVertexPf(
            complex,
            path.base,
            i._2,
            i._1
          )
        } yield newPath -> (accumPf | otherWayPf)

      normalpaths
        .find(_._1 == None)
        .map {
          case (_, trivPf) => None -> trivPf
        } // homotopically trivial
        .getOrElse {
          val plpaths = for {
            (Some(path), pathPf) <- normalpaths
            shortPathOpt = uniqrepuptoflipandcyclicper.get(path)
            _ = assert(
              shortPathOpt.nonEmpty,
              s"path $path not in uniqueUptoFlipAndCyclicPer"
            )
            shortPath <- shortPathOpt
            flipPf = FreeHomotopy.getRotationOrFlip(path, shortPath)
            spOpt = shortestPlReps.get(shortPath)
            _ = assert(spOpt.nonEmpty, s"path $shortPath not in shortestPlReps")
            sp <- spOpt
          } yield (sp, pathPf | flipPf)

          val plpBdd =
            plpaths.collect {
              case (Some(plp), pf) => plp -> pf
            }
          val newminplpathOpt = plpBdd
            .zip(plpBdd.map(_._1.length))
            .minByOption(_._2)
          newminplpathOpt
            .map {
              case ((newminplpath, pf), l) =>
                if (l < path.length)
                  shortenPf(
                    complex,
                    newminplpath,
                    sep,
                    uniqrepuptoflipandcyclicper,
                    shortestPlReps,
                    pf
                  )
                else Some(path) -> accumPf // original curve shortest
            }
            .getOrElse(Some(path) -> accumPf) // no bounded perturbations
        }
    }
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
    * @param shortestPlReps  map from the normalpaths that are enumerated to their minimal PL representatives
    * @param accum parameter for recursion
    * @return Set of isotopy classes
    */
  def postEnumIsotopyCheck(
      complex: TwoComplex[SkewPantsHexagon],
      paths: Set[PLPath],
      sep: Double,
      uniqrepuptoflipandcyclicper: Map[NormalPath[SkewPantsHexagon], NormalPath[
        SkewPantsHexagon
      ]],
      shortestPlReps: Map[NormalPath[SkewPantsHexagon], Option[PLPath]],
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
        shortestPlReps
      )
      postEnumIsotopyCheck(
        complex,
        removeGivenPaths(paths, eqclass),
        sep,
        uniqrepuptoflipandcyclicper,
        shortestPlReps,
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
    * @param shortestPlReps map from the normalpaths that are enumerated to their minimal PL representatives
    * @param accum parameter for recursion
    * @return The isotopy class
    */
  def getPostEnumIsotopyClass(
      complex: TwoComplex[SkewPantsHexagon],
      path: PLPath,
      sep: Double,
      uniqrepuptoflipandcyclicper: Map[NormalPath[SkewPantsHexagon], NormalPath[
        SkewPantsHexagon
      ]],
      shortestPlReps: Map[NormalPath[SkewPantsHexagon], Option[PLPath]],
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
        shortestPlReps
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
          shortestPlReps,
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
      sep: Double,
      tol: Double
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

  /**
    * Searches for a maximal systole over a family of surfaces specified by supplied parameters
    *
    * @param genus Genus of the surface
    * @param cuffLengthBound Upper bound for cuff lengths, lower bound is always 1
    * @param lengthStep Step for cuff lengths
    * @param twistStep Step for twists
    * @param normalPathLengthBound Upper bound for length of normalpaths
    * @param sep Separation between adjacent endpoints of PLPaths
    * @param tol Tolerance multiplier for length of closed curves over shortest pant cuff
    * @return Vector of surfaces with maximal systoles
    */
  def findMaximalSystole(
      genus: Int,
      cuffLengthBound: BigDecimal,
      lengthStep: BigDecimal,
      twistStep: BigDecimal,
      normalPathLengthBound: Int,
      sep: Double,
      tol: Double
  ): Vector[(SkewPantsSurface, Set[PLPath])] = {
    val twists = (BigDecimal(0) until BigDecimal(1) by twistStep).toVector
    val lengths = (BigDecimal(1) to cuffLengthBound by lengthStep).toVector
    val skewsurfs = (for {
      surf <- PantsSurface.allClosed((2 * genus) - 2)
      skewsurf <- SkewPantsSurface.enumerate(surf, twists, lengths)
    } yield skewsurf).filter(_.cs.exists(_.length == 1))
    val systoles = skewsurfs.zip(
      skewsurfs.map(
        s => PLPath.shortPathsfromSurface(s, normalPathLengthBound, sep, tol)
      )
    )
    val maxsize = systoles.map(_._2.size).max
    systoles.filter(_._2.size == maxsize)
  }

  def shortPathsData(
      surf: SkewPantsSurface,
      sizebound: Int,
      sep: Double,
      tol: Double
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
    sep: Double,
    tol: Double,
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
