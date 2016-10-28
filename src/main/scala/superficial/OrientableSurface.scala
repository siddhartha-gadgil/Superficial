package superficial
//import scala.collection.immutable.Vector._

sealed trait ClosedCurve
case object Identity extends ClosedCurve
sealed trait Generator extends ClosedCurve
case class Word(xs: Vector[Generator]) extends ClosedCurve

case class a(index: Int) extends Generator
case class b(index: Int) extends Generator
case class Inverse(gen: Generator) extends Generator



// As of now, considering surfaces with no boundary.
case class OrientableSurface(g: Int) {

	val d: Vector[Generator] = this.g match {
		case 1 => Vector(a(1),b(1),Inverse(a(1)),Inverse(b(1)))
		case g => OrientableSurface(g-1).d ++ Vector(a(g),b(g),Inverse(a(g)),Inverse(b(g)))
	}

	def reduce(w: Word): Word = ??? // red3(red2(red1(w.xs))) but how to implement while loop

	def red1(xs: Vector[Generator]): Vector[Generator] = xs match {
		case Vector() => Vector()
		case l1 +: Inverse(l2) +: rest if l1 == l2 => red1(rest)
		case Inverse(l1) +: l2 +: rest if l1 == l2 => red1(rest)
		case l1 +: rest :+ Inverse(l2) if l1 == l2 => red1(rest)
		case Inverse(l1) +: rest :+ l2 if l1 == l2 => red1(rest)
		case x +: rest => x +: red1(rest)
	}

	def red2(xs: Vector[Generator]): Vector[Generator] =
		if (xs.containsSlice(d)) {xs.patch(xs.indexOfSlice(d),Vector(a(3)),d.length)}
		else {Vector()}

/*what stuff i need.
patch
foldbyright(Inverse) over xs
slice
containsSlice
indexOfSlice


psuedo code:
def 2(xs) = xs++xs

def red2(w) = subred2(w,0,d.length)
def subred2(w,i,n) = {
	val j = w.indexOfSlice(2(d).slice(i,i+n))

	if ( w.containsSlice(2(d).slice(i,i+n)) )
		{ w.slice(0,j) ++ inverse(   ) ++ subred2(w.slice(j,:),i,n) }
	else if ( 2(w).containsSlice(2(d).slice(i,i+n)) )
		{                               }
	else if (i < d.length) {subred2(w,i+1,n)}
	else if (n > d.length/2) { subred2(w,0,n-1) }
	else { w }
}

*/

}
