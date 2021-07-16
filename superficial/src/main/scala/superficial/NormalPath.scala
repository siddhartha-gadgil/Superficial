package superficial

import Polygon.Index
import upickle.default.{ReadWriter => RW, macroRW, _}
import doodle.core.Vec
import scala.tools.nsc.doc.html.HtmlTags

/**
  * A normal arc in a face
  * @param initial the edge containing the initial point
  * @param terminal the edge containing the final point
  * @param face the face containing the arc
  */
case class NormalArc[P <: Polygon](initial: Index, terminal: Index, face: P) {
  override val hashCode: Int = scala.util.hashing.MurmurHash3.productHash(this)

  val terminalEdge = face.boundary(terminal)

  val initialEdge = face.boundary(initial)

  lazy val flip = NormalArc[P](terminal, initial, face)

  def vertexLinking =
    (math.abs((((terminal - initial) % face.sides) + face.sides) % face.sides) == 1) || (math
      .abs(
        (((terminal - initial) % face.sides) + face.sides) % face.sides
      ) == (face.sides - 1))

  def whichVertexLinking: Option[Vertex] =
    if (math.abs(
          (((terminal - initial) % face.sides) + face.sides) % face.sides
        ) == 1) Some(face.boundary(initial).terminal)
    else if (math.abs(
               (((terminal - initial) % face.sides) + face.sides) % face.sides
             ) == (face.sides - 1)) Some(face.boundary(terminal).terminal)
    else None

  def crosses(that: NormalArc[P]) =
    (that.initial - initial) * (that.terminal - terminal) * (that.initial - terminal) * (that.terminal - initial) < 0
}

object NormalArc {

  type cptNormalArcSPH = (Index, Index, Index, Boolean)

  def toCpt(arc: NormalArc[SkewPantsHexagon]): cptNormalArcSPH =
    (arc.initial, arc.terminal, arc.face.pants, arc.face.top)

  def fromCpt(
      cptarc: cptNormalArcSPH,
      skewsurf: SkewPantsSurface
  ): NormalArc[SkewPantsHexagon] =
    NormalArc(
      cptarc._1,
      cptarc._2,
      SkewPantsHexagon(cptarc._3, cptarc._4, skewsurf.cs)
    )

  def enumerate[P <: Polygon](complex: TwoComplex[P]): Set[NormalArc[P]] =
    for {
      face <- complex.faces
      initial <- face.indices
      terminal <- face.indices
      if terminal != initial
    } yield NormalArc(initial, terminal, face)

  /**
    * For arcs that are parallel to an edge in a SkewPantsHexagon, get the arc in the adjacent polygon
    *
    * @param complex the complex, practically a SkewPantsSurface
    * @param arc the NormalArc
    * @return the arc in the adjacent polygon
    */
  def adjacentPolygonArcs[P <: Polygon](
      complex: TwoComplex[P],
      arc: NormalArc[P]
  ): Set[NormalArc[P]] = arc.initialEdge match {
    case s1: SkewCurveEdge =>
      arc.terminalEdge match {
        case s2: SkewCurveEdge =>
          if (math.abs((arc.terminal - arc.initial) % arc.face.sides) == 2)
            getAdjacentPolygonArcs(complex, arc)
          else Set()
        case p2: PantsSeam => Set()
      }
    case p1: PantsSeam =>
      arc.terminalEdge match {
        case s2: SkewCurveEdge => Set()
        case p2: PantsSeam     => getAdjacentPolygonArcs(complex, arc)
      }
  }

  // Helper for adjacentPolygonArcs
  def getAdjacentPolygonArcs[P <: Polygon](
      complex: TwoComplex[P],
      arc: NormalArc[P]
  ): Set[NormalArc[P]] = (arc.terminal - arc.initial) % arc.face.sides match {
    case 2 =>
      val newvalues = (complex
        .edgeIndices(
          arc.face.boundary((arc.terminal - 1) % arc.face.sides)
        )
        .map {
          case (f, i, _) => (f, i)
        } -
        (arc.face -> (arc.terminal - 1) % arc.face.sides)).head
      val newarc = NormalArc(
        (newvalues._2 - 1) % newvalues._1.sides,
        (newvalues._2 + 1) % newvalues._1.sides,
        newvalues._1
      )
      Set(newarc, newarc.flip)
    case 3 =>
      val newvalues1 = (complex
        .edgeIndices(
          arc.face.boundary((arc.terminal - 1) % arc.face.sides)
        )
        .map {
          case (f, i, _) => (f, i)
        } -
        (arc.face -> (arc.terminal - 1) % arc.face.sides)).head
      val newarc1 = NormalArc(
        (newvalues1._2 - 2) % newvalues1._1.sides,
        (newvalues1._2 + 1) % newvalues1._1.sides,
        newvalues1._1
      )
      val newvalues2 = (complex
        .edgeIndices(
          arc.face.boundary((arc.terminal - 2) % arc.face.sides)
        )
        .map {
          case (f, i, _) => (f, i)
        } -
        (arc.face -> (arc.terminal - 2) % arc.face.sides)).head
      val newarc2 = NormalArc(
        (newvalues2._2 - 1) % newvalues2._1.sides,
        (newvalues2._2 + 2) % newvalues2._1.sides,
        newvalues2._1
      )
      Set(newarc1, newarc1.flip, newarc2, newarc2.flip)
    case _ => getAdjacentPolygonArcs(complex, arc.flip)
  }

  /**
    * Get arcs close to a given NormalArc in a SkewPantsSurface
    *
    * @param complex the complex, practically a SkewPantsSurface
    * @param arc the NormalArc
    * @return the neighbouring arcs
    */
  def neighbouringArcs[P <: Polygon](
      complex: TwoComplex[P],
      arc: NormalArc[P]
  ): Set[NormalArc[P]] = {
    (for {
      i1 <- Set(
        (arc.initial - 1) % arc.face.sides,
        arc.initial,
        (arc.initial + 1) % arc.face.sides
      )
      i2 <- Set(
        (arc.terminal - 1) % arc.face.sides,
        arc.terminal,
        (arc.terminal + 1) % arc.face.sides
      )
    } yield
      NormalArc(i1, i2, arc.face)).union(adjacentPolygonArcs(complex, arc))
  }

  implicit val rw: RW[NormalArc[SkewPantsHexagon]] = macroRW
}

case class NormalPath[P <: Polygon](edges: Vector[NormalArc[P]]) {
  edges.zip(edges.tail).foreach {
    case (e1, e2) =>
      require(
        e1.terminalEdge == e2.initialEdge || e1.terminalEdge == e2.initialEdge.flip,
        s"terminal point on ${e1.terminalEdge} of $e1 is not initial point of $e2 on ${e2.initialEdge}"
      )
      require(!(e1.terminalEdge.isInstanceOf[BoundaryEdge]))
  }

  override val hashCode: Int = scala.util.hashing.MurmurHash3.productHash(this)

  val length = edges.size

  lazy val flip = NormalPath[P](edges.reverse.map(_.flip))

  def +:(arc: NormalArc[P]) = NormalPath(arc +: edges)

  def :+(arc: NormalArc[P]) = NormalPath(edges :+ arc)

  def ++(newpath: NormalPath[P]) = NormalPath(edges ++ newpath.edges)
  //
  //  def appendOpt(arc: NormalArc): Option[NormalPath] =
  //    if (arc.initial == terminalEdge && arc != edges.last.flip) Some(this :+ arc)
  //    else None

  val isClosed
      : Boolean = (edges.last.terminalEdge == edges.head.initialEdge.flip)

  val isVertexLinking: Boolean = {
    val vertexlinkingdata = edges.map(_.whichVertexLinking)
    vertexlinkingdata.forall(_.isDefined) && vertexlinkingdata.forall(
      _ == vertexlinkingdata.head
    )
  }
  //  val initEdge: Edge = edges.head.initial
  //
  val (terminalFace, terminalIndex) = edges.last.face -> edges.last.terminal

  val terminalEdge =
    terminalFace.boundary(terminalIndex)

  val (initialFace, initialIndex) = edges.head.face -> edges.head.initial

  val initialEdge =
    initialFace.boundary(initialIndex)

  def distinctFaces: Boolean = edges.map(_.face).distinct.size == edges.size

  def linkingPair = edges.zip(edges.tail :+ edges.head).exists {
    case (x, y) => x.vertexLinking && y.vertexLinking
  }

  def distinctEdges = {
    val indEdg = edges.zipWithIndex
    for {
      (x, i) <- indEdg
      (y, j) <- indEdg
    } yield (x, y)
  }

  def withCross = distinctEdges.exists { case (x, y) => x.crosses(y) }

  def geodesicCandidate = !(linkingPair || withCross)
}

object NormalPath {
  implicit val rw: RW[NormalPath[SkewPantsHexagon]] = macroRW

  type cptNormalPathSPH = Vector[NormalArc.cptNormalArcSPH]

  def toCpt(path: NormalPath[SkewPantsHexagon]): cptNormalPathSPH =
    path.edges.map(arc => NormalArc.toCpt(arc))

  def fromCpt(
      cptpath: cptNormalPathSPH,
      skewsurf: SkewPantsSurface
  ): NormalPath[SkewPantsHexagon] =
    NormalPath(cptpath.map(c => NormalArc.fromCpt(c, skewsurf)))

  def optEdges[P <: Polygon](
      pathOpt: Option[NormalPath[P]]
  ): Vector[NormalArc[P]] =
    pathOpt.map(_.edges).getOrElse(Vector())

  def opt[P <: Polygon](v: Vector[NormalArc[P]]): Option[NormalPath[P]] =
    if (v.isEmpty) None else Some(NormalPath(v))

  def concat[P <: Polygon](
      first: Option[NormalPath[P]],
      second: Option[NormalPath[P]]
  ): Option[NormalPath[P]] =
    opt(optEdges(first) ++ optEdges(second))

  @annotation.tailrec
  def enumerateRec[P <: Polygon](
      complex: TwoComplex[P],
      maxAppendLength: Option[Int],
      p: NormalPath[P] => Boolean,
      latest: Set[NormalPath[P]],
      accum: Set[NormalPath[P]]
  ): Set[NormalPath[P]] = {
    if (maxAppendLength.contains(0) || latest.isEmpty) accum
    else {
      val newPaths =
        (
          for {
            path <- latest
            (face, i1) <- complex.edgeIndices(path.terminalEdge).map {
              case (f, i, _) => (f, i)
            } -
              (path.terminalFace -> path.terminalIndex)
            i2 <- face.indices
            if (i2 != i1)
            arc = NormalArc(i1, i2, face)
          } yield path :+ arc
        ).filter(endsGoAround(_).isEmpty).filter(p)
      enumerateRec(
        complex,
        maxAppendLength.map(_ - 1),
        p,
        newPaths,
        accum union newPaths
      )
    }
  }

  /**
    * recursively enumerate normal paths satisfying a hereditary condition with
    * optional bound on length;
    * terminates if the length is reached or no new paths were generated in the last step.
    * @param complex  the two-complex
    * @param maxLength an optional maximum length
    * @param p a hereditary condition
    * @return set of patbs with bounded length satisfying the condition
    */
  def enumerate[P <: Polygon](
      complex: TwoComplex[P],
      maxLength: Option[Int] = None,
      p: NormalPath[P] => Boolean = (p: NormalPath[P]) => true
  ): Set[NormalPath[P]] =
    if (maxLength.exists(_ < 1)) Set()
    else {
      val lengthOne =
        NormalArc
          .enumerate(complex)
          .map((arc) => NormalPath(Vector(arc)))
          .filter(p)
      enumerateRec(complex, maxLength.map(_ - 1), p, lengthOne, lengthOne)
    }

  /**
    * In a NormalPath, checks if the face adjacent to the face of the first NormalArc via the initialedge of the first NormalArc is the same as
    * the face adjacent to the face of the last NormalArc via the terminaledge of the last NormalArc
    *
    * @param complex the complex, practically a SkewPantsSurface
    * @param path the NormalPath
    * @return
    */
  def startEndSameFace[P <: Polygon](
      complex: TwoComplex[P],
      path: NormalPath[P]
  ): Boolean =
    //replace with more readable code
    complex
      .edgeIndices(path.edges.head.initialEdge)
      .filter {
        case (f, i, _) =>
          !((f == path.edges.head.face) && (i == path.edges.head.initial))
      }
      .map {
        case (f, _, _) => f
      }
      .head == complex
      .edgeIndices(path.edges.last.terminalEdge)
      .filter {
        case (f, i, _) =>
          !((f == path.edges.last.face) && (i == path.edges.last.terminal))
      }
      .map {
        case (f, _, _) => f
      }
      .head

  /**
    * Checks if there is a chain of NormalArcs at the end of a NormalPath that is closed and vertex-linking
    * If yes, returns the vector of arcs preceding the vertex-linking path at the end
    *
    * @param path the NormalPath
    * @return
    */
  def endsGoAround[P <: Polygon](
      path: NormalPath[P]
  ): Option[Vector[NormalArc[P]]] =
    endsGoAroundrec(
      path.edges.init,
      path.edges.last.whichVertexLinking,
      NormalPath[P](Vector(path.edges.last))
    )

  // Helper for endsGoAround
  def endsGoAroundrec[P <: Polygon](
      initedges: Vector[NormalArc[P]],
      optvertex: Option[Vertex],
      accum: NormalPath[P]
  ): Option[Vector[NormalArc[P]]] = optvertex match {
    case None => None
    case Some(v) =>
      initedges.isEmpty match {
        case true =>
          if (accum.edges.head.initialEdge == accum.edges.last.terminalEdge.flip)
            Some(initedges)
          else None
        case false =>
          initedges.last.whichVertexLinking match {
            case Some(newv) =>
              if (newv == v)
                endsGoAroundrec(
                  initedges.init,
                  optvertex,
                  NormalPath[P](initedges.last +: accum.edges)
                )
              else if (accum.edges.head.initialEdge == accum.edges.last.terminalEdge.flip)
                Some(initedges)
              else None
            case None =>
              if (accum.edges.head.initialEdge == accum.edges.last.terminalEdge.flip)
                Some(initedges)
              else None
          }
      }
  }

  /**
    * Recursively remove flips and cyclic permutations from a set of NormalPaths
    *
    * @param accum Set of paths that does not contain duplicates upto flip and cyclic permutations
    * @param paths Set of paths from which duplicates have to filtered out
    * @return Set of paths that does not contain duplicates upto flip and cyclic permutations
    */
  def removeFlipAndCyclicPerRec[P <: Polygon](
      accum: Set[NormalPath[P]],
      paths: Set[NormalPath[P]]
  ): Set[NormalPath[P]] = {
    if (paths.isEmpty) accum
    else {
      removeFlipAndCyclicPerRec(
        accum + paths.head,
        paths.tail.filter(
          p =>
            (p.edges.size != paths.head.edges.size) || ((p.edges.toSet != paths.head.edges.toSet) && (p.edges.toSet != paths.head.flip.edges.toSet))
        )
      )
    }
  }

  /**
    * Remove flips and cyclic permutations from a set of NormalPaths
    *
    * @param paths Set of paths
    * @return Set of paths from which flip and cyclic permutation duplicates have been removed
    */
  def removeFlipAndCyclicPer[P <: Polygon](
      paths: Set[NormalPath[P]]
  ): Set[NormalPath[P]] = {
    require(paths.forall(_.isClosed))
    if (paths.isEmpty) paths
    else {
      removeFlipAndCyclicPerRec(
        Set(paths.head),
        paths.tail.filter(
          p =>
            (p.edges.size != paths.head.edges.size) || ((p.edges.toSet != paths.head.edges.toSet) && (p.edges.toSet != paths.head.flip.edges.toSet))
        )
      )
    }
  }

  def rotateAndFlip[P <: Polygon](path: NormalPath[P]): Set[NormalPath[P]] = {
    val rotated = ((0 until path.edges.size)
      .map(k => NormalPath(path.edges.drop(k) ++ path.edges.take(k))))
      .toSet
    rotated union (rotated.map(_.flip))
  }

  /**
    * A map that takes each closed normalpath to its representative upto flips and cyclic permutations that will be enumerated
    *
    * @param paths Set of closed paths which constitute the domain of the map
    * @return
    */
  def uniqueUptoFlipAndCyclicPerm[P <: Polygon](
      paths: Set[NormalPath[P]]
  ): Map[NormalPath[P], NormalPath[P]] = {
    require(paths.forall(_.isClosed), "All paths are not closed")
    val groups = paths
      .groupBy(
        path => rotateAndFlip(path)
        // (path.edges.size, Set(path.edges.toSet, path.edges.map(_.flip).toSet))
      )
      .values
      .toSet
    groups.flatMap { group =>
      val chosen = group.head
      group.map(x => x -> chosen)
    }.toMap
  }

  def uniqueUptoFlipAndCyclicPermToJson(
      data: Map[NormalPath[SkewPantsHexagon], NormalPath[SkewPantsHexagon]]
  ): String = write(data)

  def uniqueUptoFlipAndCyclicPermToJsonCpt(
      data: Map[NormalPath[SkewPantsHexagon], NormalPath[SkewPantsHexagon]],
      skewsurf: SkewPantsSurface
  ): String = {
    write(
      (
        skewsurf,
        data.map(
          p => NormalPath.toCpt(p._1) -> NormalPath.toCpt(p._2)
        )
      )
    )
  }

  def uniqueUptoFlipAndCyclicPermFromJson(js: String) =
    read[Map[NormalPath[SkewPantsHexagon], NormalPath[SkewPantsHexagon]]](js)

  def uniqueUptoFlipAndCyclicPermFromJsonCpt(js: String) = {
    val cptdata = read[
      (
          SkewPantsSurface,
          Map[NormalPath.cptNormalPathSPH, NormalPath.cptNormalPathSPH]
      )
    ](js)
    cptdata._2.map(
      p =>
        NormalPath.fromCpt(p._1, cptdata._1) -> NormalPath
          .fromCpt(p._2, cptdata._1)
    )
  }

  // old version of uniqueUptoFlipAndCyclicPerm
  def uniqRepUptoFlipAndCyclicPer[P <: Polygon](
      paths: Set[NormalPath[P]]
  ): Map[NormalPath[P], NormalPath[P]] = {
    require(paths.forall(_.isClosed), "All paths are not closed")
    if (paths.isEmpty) Map()
    else {
      val diffpaths = paths.tail.filter(
        p =>
          (p.edges.size != paths.head.edges.size) || ((p.edges.toSet != paths.head.edges.toSet) && (p.edges.toSet != paths.head.flip.edges.toSet))
      )
      val newmap = paths.diff(diffpaths).map(p => (p, paths.head)).toMap
      uniqRepUptoFlipAndCyclicPerRec(newmap, diffpaths)
    }
  }
  // helper for uniqRepUptoFlipAndCyclicPer
  def uniqRepUptoFlipAndCyclicPerRec[P <: Polygon](
      accum: Map[NormalPath[P], NormalPath[P]],
      paths: Set[NormalPath[P]]
  ): Map[NormalPath[P], NormalPath[P]] =
    if (paths.isEmpty) accum
    else {
      val diffpaths = paths.tail.filter(
        p =>
          (p.edges.size != paths.head.edges.size) || ((p.edges.toSet != paths.head.edges.toSet) && (p.edges.toSet != paths.head.flip.edges.toSet))
      )
      val newmap = paths.diff(diffpaths).map(p => (p, paths.head)).toMap
      uniqRepUptoFlipAndCyclicPerRec(accum ++ newmap, diffpaths)
    }

  /**
    * Set of NormalArcs neighbouring a given NormalPath
    *
    * @param complex the complex, practically a SkewPantsSurface
    * @param path the NormalPath
    * @return Set of NormalArcs
    */
  def pathNeighbouringArcs[P <: Polygon](
      complex: TwoComplex[P],
      path: NormalPath[P]
  ): Set[NormalArc[P]] = {
    (for {
      arc <- path.edges
      nbarc <- NormalArc.neighbouringArcs(complex, arc)
    } yield nbarc).toSet
  }

  /**
    * Removes vertex linking closed subpaths from the edges of a normalpath
    * Optionally returns the resulting normalpath, with None returned if the normalpath ends up being trivial
    *
    * @param edges the edges
    * @param recedges parameter for recursion, should equal edges at the start (in general those that need to be processesed)
    * @return
    */
  def removeVertexLinkingSubPaths[P <: Polygon](
      edges: Vector[NormalArc[P]],
      recEdges: Vector[NormalArc[P]]
  ): Option[NormalPath[P]] = recEdges.isEmpty match {
    case true => if (edges.isEmpty) None else Some(NormalPath(edges))
    case false =>
      endsGoAround(NormalPath(recEdges)) match {
        case None => removeVertexLinkingSubPaths(edges, recEdges.init)
        case Some(newedges) =>
          removeVertexLinkingSubPaths(
            newedges ++ edges.diff(recEdges),
            newedges
          )
      }
  }

  // Modified implementation (for easier reasoning) with proof
  def removeVertexLinkingSubPathsPf[P <: Polygon](
      edges: Vector[NormalArc[P]],
      accum: Vector[NormalArc[P]] = Vector(),
      accumProof: PathHomotopy[P] = PathHomotopy.Const[P](None)
  ): (Option[NormalPath[P]], PathHomotopy[P]) = {
    require(
      accumProof.endEdges == accum,
      "accumulated proof must match accumulated edges"
    )
    if (edges.isEmpty) (opt(accum), accumProof)
    else
      endsGoAround(NormalPath(edges)) match {
        case None =>
          val lastPf = PathHomotopy.Const(Some(NormalPath(Vector(edges.last))))
          removeVertexLinkingSubPathsPf(
            edges.init,
            edges.last +: accum,
            lastPf * accumProof
          )
        case Some(newEdges) =>
          val dropped = edges.drop(newEdges.size)
          val vert = dropped.last.whichVertexLinking.get
          val droppedProof =
            PathHomotopy.VertexLinking(NormalPath(dropped), vert)
          removeVertexLinkingSubPathsPf(
            newEdges,
            accum,
            droppedProof * accumProof
          )
      }
  }.ensuring(
    { case (path, proof) => proof.end == path },
    s"result of homotopy must be the final surve"
  )

  /**
    * From a given closed NormalPath, recursively remove NormalArcs that go from an edge to itself and
    * replace a subsequence of NormalArcs in a single face by a single NormalArc
    *
    * @param path the NormalPath
    * @return
    */
  def makeClosedPathsTaut[P <: Polygon](
      path: NormalPath[P]
  ): Option[NormalPath[P]] = {
    import path.edges
    require(
      ((edges.last.terminalEdge == edges.head.initialEdge.flip) || (edges.last.terminalEdge == edges.head.initialEdge)),
      "Path given by edges is not closed"
    )
    val newedges = edges.filter(e => (e.initial != e.terminal))
    if (newedges.isEmpty) None
    else {
      val samefaceedgepairOpt = newedges
        .zip(newedges.tail :+ newedges.head)
        .find(p => (p._1.face == p._2.face) && (p._1.terminal == p._2.initial))
      samefaceedgepairOpt
        .map({ samefaceedgepair =>
          val indextoremove = newedges.indexOf(samefaceedgepair._1)
          if (indextoremove != (newedges.length - 1))
            makeClosedPathsTaut(
              NormalPath(
                (newedges.slice(0, indextoremove) :+ NormalArc(
                  newedges(indextoremove).initial,
                  newedges(indextoremove + 1).terminal,
                  newedges(indextoremove).face
                )) ++ newedges.drop(indextoremove + 2)
              )
            )
          else {
            makeClosedPathsTaut(
              NormalPath(
                NormalArc(
                  newedges.last.initial,
                  newedges.head.terminal,
                  newedges.head.face
                ) +: newedges.drop(1).dropRight(1)
              )
            )
          }
        })
        .getOrElse(removeVertexLinkingSubPaths(newedges, newedges))
    }
  }

  /**
    * From a given closed NormalPath, recursively remove NormalArcs that go from an edge to itself and
    * replace a subsequence of NormalArcs in a single face by a single NormalArc
    *
    * @param path the NormalPath
    * @return
    */
  def makeClosedPathsTautPf[P <: Polygon](
      path: NormalPath[P],
      accumPf: FreeHomotopy[P]
  ): (Option[NormalPath[P]], FreeHomotopy[P]) = {
    require(
      accumPf.end == Some(path),
      "accumulated proof should give homotopy to given path"
    )
    import path.edges
    require(
      ((edges.last.terminalEdge == edges.head.initialEdge.flip) || (edges.last.terminalEdge == edges.head.initialEdge)),
      "Path given by edges is not closed"
    ) // implicitly assuming path is not empty
    val newedges: Vector[NormalArc[P]] =
      edges.filter(e => (e.initial != e.terminal))
    val filterPfVec: Vector[PathHomotopy[P]] = edges.map(
      e =>
        if (e.initial != e.terminal)
          PathHomotopy.Const(Some(NormalPath(Vector(e))))
        else PathHomotopy.EdgeNbd(e)
    )
    val filterPf: FreeHomotopy[P] = filterPfVec.reduce(_ * _).free
    if (newedges.isEmpty) (None, filterPf)
    else {
      val samefaceedgepairOpt = newedges
        .zip(newedges.tail :+ newedges.head)
        .find(p => (p._1.face == p._2.face) && (p._1.terminal == p._2.initial))
      samefaceedgepairOpt
        .map({ samefaceedgepair =>
          val indextoremove = newedges.indexOf(samefaceedgepair._1)
          if (indextoremove != (newedges.length - 1)) {
            val newArc = NormalArc(
              newedges(indextoremove).initial,
              newedges(indextoremove + 1).terminal,
              newedges(indextoremove).face
            )
            val mergePiecePf = PathHomotopy.InFace(
              NormalPath(newedges.drop(indextoremove).take(2)),
              NormalPath(Vector(newArc)),
              newArc.face
            )
            val mergePf = (PathHomotopy
              .Const(opt(newedges.take(indextoremove))) * mergePiecePf * PathHomotopy
              .Const(opt(newedges.drop(indextoremove + 2)))).free
            makeClosedPathsTautPf(
              NormalPath(
                (newedges.slice(0, indextoremove) :+ newArc) ++ newedges
                  .drop(indextoremove + 2)
              ),
              accumPf | filterPf | mergePf
            )
          } else {
            {
              val newArc = NormalArc(
                newedges.last.initial,
                newedges.head.terminal,
                newedges.head.face
              )
              val mergePiecePf = PathHomotopy.InFace(
                NormalPath(Vector(newedges.last, newedges.head)),
                NormalPath(Vector(newArc)),
                newArc.face
              )
              val mergePf = FreeHomotopy
                .shifRight(NormalPath(newedges), 1) | (mergePiecePf *
                PathHomotopy
                  .Const(opt(newedges.drop(1).dropRight(1)))).free
              makeClosedPathsTautPf(
                NormalPath(
                  newArc +: newedges.drop(1).dropRight(1)
                ),
                accumPf | filterPf | mergePf
              )
            }
          }
        })
        .getOrElse {
          val (newPath, purgePf) = removeVertexLinkingSubPathsPf(newedges)
          (newPath, accumPf | filterPf | purgePf.free)
        }
    }
  }

  /**
    * Outputs a vertex linking path
    *
    * @param complex the surface
    * @param startingface face containing the first arc of the path
    * @param startingindex initial index of the first arc of the path
    * @param linkinitialvertex whether the path links the initial vertex of the initial edge of the first arc (otherwise final vertex is linked)
    * @return A vertex linking path
    */
  def vertexLinkingPath[P <: Polygon](
      complex: TwoComplex[P],
      startingface: P,
      startingindex: Index,
      linkinitialvertex: Boolean
  ): NormalPath[P] = {
    if (linkinitialvertex)
      vertexLinkingPathRec(
        complex,
        NormalPath(
          Vector(
            NormalArc(
              startingindex,
              (startingindex - 1 + startingface.sides) % startingface.sides,
              startingface
            )
          )
        ),
        -1
      )
    else
      vertexLinkingPathRec(
        complex,
        NormalPath(
          Vector(
            NormalArc(
              startingindex,
              (startingindex + 1) % startingface.sides,
              startingface
            )
          )
        ),
        1
      )
  }

  //helper for vertex linking path
  def vertexLinkingPathRec[P <: Polygon](
      complex: TwoComplex[P],
      accum: NormalPath[P],
      arcedgeshift: Int
  ): NormalPath[P] =
    if (accum.edges.last.terminalEdge.flip == accum.edges.head.initialEdge)
      accum
    else {
      val newarcface: P = complex.faces
        .find(_.boundary.contains(accum.edges.last.terminalEdge.flip))
        .get
      val newarcinitial =
        newarcface.boundary.indexOf(accum.edges.last.terminalEdge.flip)
      vertexLinkingPathRec(
        complex,
        accum.:+(
          NormalArc(
            newarcinitial,
            (newarcinitial + arcedgeshift + newarcface.sides) % newarcface.sides,
            newarcface
          )
        ),
        arcedgeshift
      )
    }

  /**
    * Moves a normalpath across to the other side of a vertex
    * Return is Optional, None is returned if the new path is contractible
    *
    * @param complex the surface
    * @param path the path
    * @param indextomove index of the arc whose terminal edge contains the vertex the path has to be moved across
    * @param lefttoright whether to move the path from the left side to the right side of a vertex
    * @return
    */
  def otherWayAroundVertex[P <: Polygon](
      complex: TwoComplex[P],
      path: NormalPath[P],
      indextomove: Index,
      lefttoright: Boolean
  ): Option[NormalPath[P]] = {
    val vertexlinkingpath = vertexLinkingPath(
      complex,
      path.edges(indextomove).face,
      path.edges(indextomove).terminal,
      lefttoright
    )
    if (indextomove != (path.edges.size - 1))
      makeClosedPathsTaut(
        NormalPath(
          path.edges
            .slice(0, indextomove + 1) ++ vertexlinkingpath.edges ++ path.edges
            .slice(indextomove + 1, path.edges.size)
        )
      )
    else makeClosedPathsTaut(NormalPath(path.edges ++ vertexlinkingpath.edges))
  }

  def otherWayAroundVertexPf[P <: Polygon](
      complex: TwoComplex[P],
      path: NormalPath[P],
      indextomove: Index,
      lefttoright: Boolean
  ): (Option[NormalPath[P]], FreeHomotopy[P]) = {
    val face = path.edges(indextomove).face
    val initIndex = path.edges(indextomove).terminal
    val edge = face.boundary(initIndex)
    val vertex = if (lefttoright) edge.initial else edge.terminal
    val vertexlinkingpath = vertexLinkingPath(
      complex,
      face,
      initIndex,
      lefttoright
    )
    val vertexPf = PathHomotopy.VertexLinking(vertexlinkingpath, vertex)
    if (indextomove != (path.edges.size - 1)) {
      val preEdges: Vector[NormalArc[P]] = path.edges
        .slice(0, indextomove + 1)
      val postEdges: Vector[NormalArc[P]] = path.edges
        .slice(indextomove + 1, path.edges.size)
      val insertPf = PathHomotopy.Const(opt(preEdges)) * vertexPf.homotopyFlip * PathHomotopy
        .Const(opt(postEdges))

      val (resPath, tautPf) = makeClosedPathsTautPf(
        NormalPath(
          preEdges ++ vertexlinkingpath.edges ++ postEdges
        ),
        insertPf.free
      )
      resPath -> tautPf
    } else {
      val insertPf: PathHomotopy[P] = PathHomotopy.Const(Some(path)) * vertexPf.homotopyFlip
      val (resPath, tautPf) =
        makeClosedPathsTautPf(
          NormalPath(path.edges ++ vertexlinkingpath.edges),
          insertPf.free
        )
      resPath -> tautPf
    }
  }

  /**
    * shorten a normal-path which crosses a vertex by replacing three arcs with two arcs
    *
    * @param complex the surface
    * @param path the path
    * @param shortestedgeindex the index of the middle arc out of the three arcs
    * @return shortened path
    */
  @deprecated("using otherWayAroundInstead")
  def shortenPathCrossingVertex(
      complex: TwoComplex[SkewPantsHexagon],
      path: NormalPath[SkewPantsHexagon],
      shortestedgeindex: Index
  ): NormalPath[SkewPantsHexagon] = {
    val shortestedge = path.edges(shortestedgeindex)
    if (shortestedge.whichVertexLinking.get == shortestedge.initialEdge.terminal)
      surgery(complex, path, shortestedgeindex, -1)
    else surgery(complex, path, shortestedgeindex, 1)
  }

  // Helper for shortening path by crossing a vertex
  @deprecated("using otherWayAroundInstead")
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
        ) ++ path.edges.drop(shortestedgeindex + 2)
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
      (((arc1.terminal + arc1edgeshift) % arc1.face.sides) + arc1.face.sides) % arc1.face.sides,
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

  /**
    * Shortens a NormalPath by removing NormalArcs which go between adjacent SkewCurveEdges
    *
    * @param complex the complex, practically a SkewPantsSurface
    * @param path the NormalPath
    * @return the shortened path
    */
  @deprecated("Use other way around vertex")
  def removeArcBetweenAdjacentSkewCurveEdges(
      complex: TwoComplex[SkewPantsHexagon],
      path: NormalPath[SkewPantsHexagon]
  ): NormalPath[SkewPantsHexagon] = {
    require(path.isClosed, s"$path is not closed")
    path.edges.find(
      arc =>
        SkewPantsHexagon
          .adjacentSkewCurveEdges(arc.face, arc.initial, arc.terminal)
    ) match {
      case None => path
      case Some(arc) =>
        removeArcBetweenAdjacentSkewCurveEdges(
          complex,
          shortenPathCrossingVertex(complex, path, path.edges.indexOf(arc))
        )
    }
  }
}

import NormalPath._

sealed trait PathHomotopy[P <: Polygon] {
  val start: Option[NormalPath[P]]

  val end: Option[NormalPath[P]]

  lazy val startEdges = optEdges(start)

  lazy val endEdges = optEdges(end)

  def |(that: PathHomotopy[P]) = PathHomotopy.HomotopyProduct(this, that)

  def *(that: PathHomotopy[P]) = PathHomotopy.PathwiseProduct(this, that)

  lazy val homotopyFlip = PathHomotopy.HomotopyFlip(this)

  lazy val pathFlip = PathHomotopy.PathwiseFlip(this)

  lazy val free: FreeHomotopy[P] = FreeHomotopy.LoopHomotopy(this)
}

object PathHomotopy {

  case class VertexLinking[P <: Polygon](
      curve: NormalPath[P],
      vertex: Vertex
  ) extends PathHomotopy[P] {
    require(
      {
        (curve.edges.forall(
          arc =>
            arc.initialEdge.initial == vertex && arc.terminalEdge.terminal == vertex
        )) ||
        (curve.edges.forall(
          arc =>
            arc.initialEdge.terminal == vertex && arc.terminalEdge.initial == vertex
        ))

      },
      s"""
          |vertex: $vertex
          |initial-initial=${curve.edges.map(_.initialEdge.initial).distinct}
          |initial-terminal=${curve.edges.map(_.initialEdge.terminal).distinct}
          |terminal-terminal=${curve.edges
           .map(_.terminalEdge.terminal)
           .distinct}
          |terminal-initial=${curve.edges
           .map(_.terminalEdge.initial)
           .distinct}""".stripMargin
    )
    val start: Option[NormalPath[P]] = Some(curve)

    val end: Option[NormalPath[P]] = None

  }

  case class EdgeNbd[P <: Polygon](arc: NormalArc[P]) extends PathHomotopy[P] {
    val start: Option[NormalPath[P]] = Some(
      NormalPath(Vector(arc))
    )

    val end: Option[NormalPath[P]] = None

    require(arc.initial == arc.terminal)

  }

  case class InFace[P <: Polygon](
      first: NormalPath[P],
      second: NormalPath[P],
      face: P
  ) extends PathHomotopy[P] {
    val start: Option[NormalPath[P]] = Some(first)

    val end: Option[NormalPath[P]] = Some(second)

    require {
      first.initialIndex == second.initialIndex && first.terminalIndex == second.terminalIndex &&
      first.edges.forall(e => e.face == face) &&
      second.edges.forall(e => e.face == face)
    }

  }
  case class Const[P <: Polygon](curveOpt: Option[NormalPath[P]])
      extends PathHomotopy[P] {
    val start: Option[NormalPath[P]] = curveOpt

    val end: Option[NormalPath[P]] = curveOpt

    override lazy val free: FreeHomotopy[P] = FreeHomotopy.Const(curveOpt.get)

  }
  case class PathwiseProduct[P <: Polygon](
      first: PathHomotopy[P],
      second: PathHomotopy[P]
  ) extends PathHomotopy[P] {
    val start: Option[NormalPath[P]] = concat(first.start, second.start)

    val end: Option[NormalPath[P]] = concat(first.end, second.end)

  }

  case class HomotopyProduct[P <: Polygon](
      first: PathHomotopy[P],
      second: PathHomotopy[P]
  ) extends PathHomotopy[P] {
    val start: Option[NormalPath[P]] = first.start

    val end: Option[NormalPath[P]] = second.end

    override lazy val free: FreeHomotopy[P] = first.free | second.free

    require(first.end == second.start)
  }

  case class HomotopyFlip[P <: Polygon](homotopy: PathHomotopy[P])
      extends PathHomotopy[P] {
    val start: Option[NormalPath[P]] = homotopy.end

    val end: Option[NormalPath[P]] = homotopy.start

  }

  case class PathwiseFlip[P <: Polygon](homotopy: PathHomotopy[P])
      extends PathHomotopy[P] {
    val start: Option[NormalPath[P]] = homotopy.start.map(_.flip)

    val end: Option[NormalPath[P]] = homotopy.end.map(_.flip)

  }

}

sealed trait FreeHomotopy[P <: Polygon] {
  val start: Option[NormalPath[P]]

  val end: Option[NormalPath[P]]

  def |(that: FreeHomotopy[P]): FreeHomotopy[P] =
    FreeHomotopy.HomotopyProduct(this, that)
}

object FreeHomotopy {
  case class LoopHomotopy[P <: Polygon](homotopy: PathHomotopy[P])
      extends FreeHomotopy[P] {
    val start: Option[NormalPath[P]] = homotopy.start

    val end: Option[NormalPath[P]] = homotopy.end

    start.foreach { path =>
      import path.edges
      require(
        ((edges.last.terminalEdge == edges.head.initialEdge.flip) || (edges.last.terminalEdge == edges.head.initialEdge)),
        "Path given by edges is not closed"
      )
    }
  }

  case class Const[P <: Polygon](path: NormalPath[P]) extends FreeHomotopy[P] {
    val start: Option[NormalPath[P]] = Some(path)

    val end: Option[NormalPath[P]] = Some(path)

    import path.edges
    require(
      ((edges.last.terminalEdge == edges.head.initialEdge.flip) || (edges.last.terminalEdge == edges.head.initialEdge)),
      "Path given by edges is not closed"
    )

    override def |(that: FreeHomotopy[P]): FreeHomotopy[P] = that
  }

  case class ShiftLeft[P <: Polygon](path: NormalPath[P], shift: Int)
      extends FreeHomotopy[P] {
    val start: Option[NormalPath[P]] = Some(path)

    val end: Option[NormalPath[P]] = Some(
      NormalPath(path.edges.drop(shift) ++ path.edges.take(shift))
    )

    import path.edges
    require(
      ((edges.last.terminalEdge == edges.head.initialEdge.flip) || (edges.last.terminalEdge == edges.head.initialEdge)),
      "Path given by edges is not closed"
    )
  }

  def shiftLeft[P <: Polygon](path: NormalPath[P], shift: Int) =
    ShiftLeft(path, shift)

  def shifRight[P <: Polygon](path: NormalPath[P], shift: Int) =
    ShiftLeft(path, path.edges.size - shift)

  case class PathwiseFlip[P <: Polygon](homotopy: FreeHomotopy[P])
      extends FreeHomotopy[P] {
    val start: Option[NormalPath[P]] = homotopy.start.map(_.flip)

    val end: Option[NormalPath[P]] = homotopy.end.map(_.flip)

  }

  case class HomotopyFlip[P <: Polygon](homotopy: FreeHomotopy[P])
      extends FreeHomotopy[P] {
    val start: Option[NormalPath[P]] = homotopy.end

    val end: Option[NormalPath[P]] = homotopy.start

  }

  case class HomotopyProduct[P <: Polygon](
      first: FreeHomotopy[P],
      second: FreeHomotopy[P]
  ) extends FreeHomotopy[P] {
    val start: Option[NormalPath[P]] = first.start

    val end: Option[NormalPath[P]] = second.end

    require(
      first.end == second.start,
      s"${first.end}\n does not match \n${second.start} \nwhile gluing"
    )
  }

  case class ReverseOrientation[P <: Polygon](path: NormalPath[P])
      extends FreeHomotopy[P] {
    val start: Option[NormalPath[P]] = Some(path)

    val end: Option[NormalPath[P]] = Some(path.flip)

    import path.edges
    require(
      ((edges.last.terminalEdge == edges.head.initialEdge.flip) || (edges.last.terminalEdge == edges.head.initialEdge)),
      "Path given by edges is not closed"
    )
  }

  def findRotation[P <: Polygon](
      source: NormalPath[P],
      target: NormalPath[P]
  ): Option[FreeHomotopy[P]] =
    (0 until source.edges.size)
      .find(k => source.edges.drop(k) ++ source.edges.take(k) == target.edges)
      .map { j =>
        shiftLeft(source, j)
      }

  def getRotationOrFlip[P <: Polygon](
      source: NormalPath[P],
      target: NormalPath[P]
  ): FreeHomotopy[P] =
    findRotation(source, target).getOrElse(
      ReverseOrientation(source) | findRotation(source.flip, target).get
    )

}
