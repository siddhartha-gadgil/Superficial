import superficial._

import Examples.doubleTriangle

// Two triangles glued in one edge
doubleTriangle

// Normal paths with length at most 3.
val paths = NormalPath.enumerate(doubleTriangle, Some(3))

val longPaths = paths.filter(p => p.edges.map(_.face).distinct.size > 1).toVector

// paths of length two start on an outer edge, go through the common edge and end on an outer edge of the other face
longPaths.size
println(longPaths.head)


paths.filter(p => p.edges.map(_.face).distinct.size > 1) == paths.filter(p => p.edges.map(_.face).size > 1)

paths.forall(p => p.edges.map(_.face).size < 3)

val paths4 = NormalPath.enumerate(doubleTriangle, Some(4))
(paths4 -- paths).isEmpty
