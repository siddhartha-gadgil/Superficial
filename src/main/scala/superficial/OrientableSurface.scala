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

	val relation: Vector[Generator] = this.g match {
		case 1 => Vector(a(1),b(1),Inverse(a(1)),Inverse(b(1)))
		case g => OrientableSurface(g-1).relation ++ Vector(a(g),b(g),Inverse(a(g)),Inverse(b(g)))
	}
	val d = relation.length

	def reduce(w: Word): Word = ??? // red3(red2(red1(w.xs)))

	def red1(xs: Vector[Generator]): Vector[Generator] = xs match {
		case Vector() => Vector()
		case l1 +: Inverse(l2) +: rest if l1 == l2 => red1(rest)
		case Inverse(l1) +: l2 +: rest if l1 == l2 => red1(rest)
		case l1 +: rest :+ Inverse(l2) if l1 == l2 => red1(rest)
		case Inverse(l1) +: rest :+ l2 if l1 == l2 => red1(rest)
		case x +: rest => x +: red1(rest)
	}

def red2(xs: Vector[Generator]) = subred2(xs,0,d)

def inv(xs: Vector[Generator]) : Vector[Generator] = xs match {
	case Vector() => Vector()
	case Inverse(l) +: rest => inv(rest) :+ l
	case l +: rest => inv(rest) :+ Inverse(l)
}

def relInv(xs: Vector[Generator]): Vector[Generator] = {
	val j = (relation++relation).indexOfSlice(xs)
	inv((relation++relation).slice(j+xs.length,j+d))
}

def subred2(xs:Vector[Generator], i:Int, n:Int): Vector[Generator] = {
	if (n > d/2) {
		if (i < d) {
			if ( (xs++xs).containsSlice((relation++relation).slice(i,i+n)) ) {
				val j = (xs++xs).indexOfSlice((relation++relation).slice(i,i+n))
				val l = xs.length
				if (j+n > l-1) { subred2( (xs++xs).slice(j+n+1-l,j) ++ relInv((xs++xs).slice(j,j+n)), i,n) }
				else { subred2( xs.slice(0,j) ++ relInv(xs.slice(j,j+n)) ++ xs.slice(j+n,l-1) , i,n) }
			}
			else { subred2(xs,i+1,n) }
		}
		else { subred2(xs,0,n-1) }
	}
	else { xs }
}

}
