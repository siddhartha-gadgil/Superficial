import superficial._

val genus2 = PantsSurface.allClosed(2).toVector
genus2.size

genus2.forall(_.isClosedSurface) 

val surf = genus2.head

surf.edges.map(surf.edgeOccurences(_))

val badEdges = surf.edges.filter(e => surf.edgeOccurences(e) == 0)

badEdges.size

surf.edges.size

surf.edges.mkString("\n")

surf.faces.size

surf.faces.toVector.map(_.checkBoundary)

surf.faces.toVector.map(_.boundary.size)

surf.faces.find(!_.checkBoundary)

surf.faces.find(!_.checkBoundary).map(_.boundary)

val genus3 = PantsSurface.allClosed(4).toVector
genus3.size

genus3.forall(_.isClosedSurface) 