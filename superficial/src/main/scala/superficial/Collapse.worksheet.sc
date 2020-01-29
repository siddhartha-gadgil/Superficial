import superficial._

case object X extends Vertex
case object Y extends Vertex
case object Z extends Vertex

case object A extends EdgePair(X, Y)
case object B extends EdgePair(Y, Z)
case object C extends EdgePair(Z, X)

val upper = Polygon(Vector(A.Positive, B.Positive, C.Positive))
val lower = Polygon(Vector(C.Negative, B.Negative, A.Negative))

val doubleTriangle: TwoComplex = TwoComplex.pure(upper, lower)

val doubleBigon = doubleTriangle.collapseEdge(A.Positive)
doubleBigon.edges
doubleBigon.vertices
doubleBigon.faces

val doubleMonogon = TwoComplex.allCollapsed(doubleTriangle)
doubleMonogon.edges
doubleMonogon.vertices
doubleMonogon.faces

doubleMonogon.edges.map{e => (e.initial, e.terminal)}
doubleBigon.edges.map{e => (e.initial, e.terminal)}

doubleMonogon.faces.map(_.boundary)
doubleBigon.faces.map(_.boundary)

doubleMonogon.positiveEdges

val faceMerged = TwoComplex.mergeFaces(C.Positive, upper, lower)
faceMerged.faces 
faceMerged.faces.map(_.boundary)

val ep = EdgePath.Append(EdgePath.Constant(X), A.Positive)
import scala.util.Try

val bad = Try{
    EdgePath.Append(EdgePath.Constant(X), A.Negative)
}
