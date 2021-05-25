package superficial

import Polygon.Index

/**
  * A normal arc in a face
  * @param initial the edge containing the initial point
  * @param terminal the edge containing the final point
  * @param face the face containing the arc
  */
case class NormalArc[P <: Polygon](initial: Index, terminal: Index, face: P) {
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
            if (i2 != i1) && (!SkewPantsHexagon.adjacentSkewCurveEdges(
              face,
              i1,
              i2
            ))
            arc = NormalArc(i1, i2, face)
          } yield path :+ arc
        ).filter(path => !(endsGoAround(complex, path))).filter(p)
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
          .filter(
            a =>
              !SkewPantsHexagon.adjacentSkewCurveEdges(
                a.face,
                a.initial,
                a.terminal
              )
          )
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
    * Checks if there is a chain of NormalArcs at the end of a NormalPath that just go around a vertex to come back to the same face
    *
    * @param complex the complex, practically a SkewPantsSurface
    * @param path the NormalPath
    * @return
    */
  def endsGoAround[P <: Polygon](
      complex: TwoComplex[P],
      path: NormalPath[P]
  ): Boolean =
    endsGoAroundrec(
      complex,
      path.edges.init,
      path.edges.last.whichVertexLinking,
      NormalPath[P](Vector(path.edges.last))
    )

  // Helper for endsGoAround
  def endsGoAroundrec[P <: Polygon](
      complex: TwoComplex[P],
      initedges: Vector[NormalArc[P]],
      optvertex: Option[Vertex],
      accum: NormalPath[P]
  ): Boolean = optvertex match {
    case None => false
    case Some(v) =>
      initedges.isEmpty match {
        case true => startEndSameFace(complex, accum)
        case false =>
          initedges.last.whichVertexLinking match {
            case Some(newv) =>
              if (newv == v)
                endsGoAroundrec(
                  complex,
                  initedges.init,
                  optvertex,
                  NormalPath[P](initedges.last +: accum.edges)
                )
              else startEndSameFace(complex, accum)
            case None => startEndSameFace(complex, accum)
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
      val samefaceedgepair = newedges
        .zip(newedges.tail :+ newedges.head)
        .find(p => (p._1.face == p._2.face) && (p._1.terminal == p._2.initial))
      if (samefaceedgepair.isEmpty) Some(NormalPath(newedges))
      else {
        val indextoremove = newedges.indexOf(samefaceedgepair.get._1)
        if (indextoremove != (newedges.length - 1))
          makeClosedPathsTaut(
            NormalPath(
              (edges.slice(0, indextoremove) :+ NormalArc(
                edges(indextoremove).initial,
                edges(indextoremove + 1).terminal,
                edges(indextoremove).face
              )) ++ edges.drop(indextoremove + 2)
            )
          )
        else {
          makeClosedPathsTaut(
            NormalPath(
              NormalArc(
                edges.last.initial,
                edges.head.terminal,
                edges.head.face
              ) +: edges.drop(1).dropRight(1)
            )
          )
        }
      }
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
