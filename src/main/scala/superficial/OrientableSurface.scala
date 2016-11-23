package superficial

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
		val nxt = red3(red2(red1(w.xs)))
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
					subred2( relInv((xs++xs).slice(j,j+n)) ++ (xs++xs).slice(j+n,j+l), i,n)
				}
				else { subred2(xs,i+1,n) }
			}
			else { subred2(xs,0,n-1) }
		}
		else { xs }
	}

	def red3(xs: Vector[Generator]): Vector[Generator] = subred3(xs,3)

	def subred3(xs: Vector[Generator], i: Int): Vector[Generator] = {
		//i is the starting index in relation of the half of relation being searched for in xs
		if (i <= d/2 )
		{
			val j = (xs++xs).indexOfSlice((relation).slice(i,i+(d/2)))
			//j is the starting index of that half of relation in xs++xs
			if ((xs++xs).containsSlice((relation).slice(i,i+(d/2)))
			&& !(xs++xs).slice(j,j+(d/2)).containsSlice(Vector(a(1)))
			&& !(xs++xs).slice(j,j+(d/2)).containsSlice(Vector(Inverse(a(1)))))
				//that is, if xs++xs contains half of relation &
				//that half does not contain a(1) or its inverse
				subred3(relInv((xs++xs).slice(j,j+(d/2))) ++ (xs++xs).slice(j+(d/2),j+(xs.length)), i)
			else subred3(xs, i+1)
		}
		else xs
	}


/// winding number and isSimple

	val greekR: Vector[Generator] = {
		if (g < 1) Vector()
		else OrientableSurface(g-1).greekR ++ Vector(a(g),Inverse(b(g)),Inverse(a(g)),b(g))
	}

	def countRespectingR(xs: Vector[Generator]) : Int = {
		val vecOfIndices = (0 to (xs.length-2)).toVector
		def respectsR(m: Int) : Boolean = xs(m) match {
			case Inverse(l) => greekR.indexOf(l) < greekR.indexOf(xs(m+1))
			case l => greekR.indexOf(Inverse(l)) < greekR.indexOf(xs(m+1))
		}

		if (xs.length < 2) 0
		else vecOfIndices.count(respectsR)
	}

 	val vecInvA = (1 to g).map((x:Int) => Inverse(a(x)))
	val vecB = (1 to g).map((x:Int) => b(x))

	def windingT(w: Word) = {
		if (w.xs.length < 2) 0
		else countRespectingR(w.xs) - (
					w.xs.count((x:Generator) => vecInvA.contains(x)) +
				 	w.xs.count((x:Generator) => vecB.contains(x)) )
	}


	def isSimple(w: Word): Boolean = {
		val reduced = reduce(w).xs

		def satisfyEquation(i:Int,j:Int): Boolean = {
			val l = reduced.length
			val permutated = (reduced++reduced).slice(i,i+l)
			val u = permutated.slice(0,j+1) // j varies from 0 to l-2 and, i varies from 0 to l/2
			val v = permutated.slice(j+1,l)

			if (i <= l/2) {
				if (j <= l-2 ) {
					if ( windingT(Word(red1(u++inv(v)))) - windingT(Word(red1(u))) - windingT(Word(red1(inv(v)))) == 0 )
					satisfyEquation(i,j+1)
					else false
				}
				else satisfyEquation(i+1,0)
			}
			else true
		}

		if (reduced.length < 2) true
		else if ( reduced.forall((x:Generator) => x == reduced(0)) ) false
		else satisfyEquation(0,0)

	}


}
