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

	val relation: Vector[Generator] = {
		if (g < 1) Vector()
		else OrientableSurface(g-1).relation ++ Vector(a(g),b(g),Inverse(a(g)),Inverse(b(g)))
	}
	val d = relation.length

	def reduce(w: Word): Word = {
		val nxt = red2(red1(w.xs))
		if (nxt == w.xs) Word(nxt) else reduce(Word(nxt))
	}

	def red1(xs : Vector[Generator]): Vector[Generator] = {
		val rw = subred1(xs)
		if (rw.length < 2) rw
		else if ( rw.head==Inverse(rw.last) | rw.last==Inverse(rw.head) )
			rw.drop(1).dropRight(1)
		else  rw
	}

	def subred1(xs: Vector[Generator]): Vector[Generator] = {
		if (xs.length < 2) xs
		else if (xs(0)==Inverse(xs(1)) | xs(1)==Inverse(xs(0)))
			subred1(xs.drop(2))
		else xs(0) +: subred1(xs.drop(1))
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
					if (j+n > l) { subred2( (xs++xs).slice(j+n+1-l,j) ++ relInv((xs++xs).slice(j,j+n)), i,n) }
					else { subred2( xs.slice(0,j) ++ relInv(xs.slice(j,j+n)) ++ xs.slice(j+n,l-1) , i,n) }
				}
				else { subred2(xs,i+1,n) }
			}
			else { subred2(xs,0,n-1) }
		}
		else { xs }
	}

}
