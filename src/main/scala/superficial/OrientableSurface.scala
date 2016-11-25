/** This program basically abstracts :
 	*		(i) OrientableSurface (i.e. a connected compact 2-manifold) parametrized by its genus g.
	*		(ii) Surfaces' fundamental group Generators along with the 'relation' over them,
	*		(iii) Closed curves/Words (on the surfaces) in terms of Generators,
	*	And, verifies whether a given word is homotopic to a simple closed curve.
	*
	* 	NOTE: As of now, program is only valid for surfaces without boundary.
	*					Its extension for surfaces with boundary, is yet to complete.
	*
	* 	Reference:: "Simple Closed Curves on Surfaces" by D.R.J. Chillingworth
	*/
package superficial

sealed trait Generator
case class a(index: Int) extends Generator
case class b(index: Int) extends Generator
case class Inverse(gen: Generator) extends Generator
/** 'a' and 'b' are the two Generators of a OrientableSurface, parametrized by the genus number.
	*	Because their inverse can also generate the fundamental group;
	* therefore they are also being considered as Generators.
	*/

case class Word(xs: Vector[Generator])
/** Here, word/closed curve is simply a list of genrators.
	*	List with one element represents Simple closed curve, and
	* empty list represents the null element or precisely, a closed curve homotopic to a point.
	*/

case class OrientableSurface(g: Int) {

	val relation: Vector[Generator] = {
		if (g < 1) Vector()
		else OrientableSurface(g-1).relation ++ Vector(a(g),b(g),Inverse(a(g)),Inverse(b(g)))
	}
	val d = relation.length

	def reduce(w: Word): Word = {
	/** This keeps on reducing w by red1,red2,red3 in order,
		*	until it starts producing the same result.
		*/
		val nxt = red3(red2(red1(w.xs)))
		if (nxt == w.xs) Word(nxt) else reduce(Word(nxt))
	}

	def red1(xs : Vector[Generator]): Vector[Generator] = {
	/** This removes all consecutive pairs (consisting element and its inverse) from the word,
	 	* by the help of subred1 and by further removing first and last element if they are forming the pair.
		* red1 is identical to cyclic reduction of a word.
		*/
		val rw = subred1(xs)
		if (rw.length < 2) rw
		else if ( rw.head==Inverse(rw.last) | rw.last==Inverse(rw.head) )
			rw.drop(1).dropRight(1)
		else  rw
	}

	def subred1(xs: Vector[Generator]): Vector[Generator] = {
	/** This removes first two element if they form such pair and calls itself over the remaining.
		* Otherwise joins the first element with its result over xs minus first element.
		*/
		if (xs.length < 2) xs
		else if (xs(0)==Inverse(xs(1)) | xs(1)==Inverse(xs(0)))
			subred1(xs.drop(2))
		else xs(0) +: subred1(xs.drop(1))
	}

	def red2(xs: Vector[Generator]) = subred2(xs,0,d)

	def inv(xs: Vector[Generator]) : Vector[Generator] = xs match {
	/** This replaces elements of xs with their inverse.
		*/
		case Vector() => Vector()
		case Inverse(l) +: rest => inv(rest) :+ l
		case l +: rest => inv(rest) :+ Inverse(l)
	 }

	def relInv(xs: Vector[Generator]): Vector[Generator] = {
	/** This returns the inverse of xs in "relation".
		*	NOTE: This assumes xs to be a subword of "relation".
		*/
		val j = (relation++relation).indexOfSlice(xs)
		inv((relation++relation).slice(j+xs.length,j+d))
	}

	def subred2(xs:Vector[Generator], i:Int, n:Int): Vector[Generator] = {
	/** Idea: Given a subword of relation, one can look for its presence in the given word,
		* 			and therefore can replace it with its relInv.
		*
		* Here, subword is taken from relation++relation (instead of relation) and
		* its presence is looked in xs++xs (instead of xs); to account for cyclic permutation.
		*	'i' is the start index of subword in relation. 0 <= i <= d
		* 'n' is the length of the subword. d >= n > d/2
		*
		*	This looks for all the subwords (of relation with length more than half of relation's) in xs and
		* replaces it with its relInv.
		*/
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
	/** Similar to subred2, this looks for all the subwords (of relation with lenght exactly half of relation)
		* in xs, which do not contain a(1) or its inverse, and replaces them with their relInv.
		*
		* 'i' is the starting index of the subword in relation. 3 <= i <= d/2
		*/
		if (i <= d/2 )
		{
			val j = (xs++xs).indexOfSlice((relation).slice(i,i+(d/2)))
			if ((xs++xs).containsSlice((relation).slice(i,i+(d/2)))
			&& !(xs++xs).slice(j,j+(d/2)).containsSlice(Vector(a(1)))
			&& !(xs++xs).slice(j,j+(d/2)).containsSlice(Vector(Inverse(a(1)))))
				subred3(relInv((xs++xs).slice(j,j+(d/2))) ++ (xs++xs).slice(j+(d/2),j+(xs.length)), i)
			else subred3(xs, i+1)
		}
		else xs
	}


	val vecInvA = (1 to g).map((x:Int) => Inverse(a(x)))
	// list of all Inverse(a(_))

	val vecB = (1 to g).map((x:Int) => b(x))
	// list of all b(_)

	val greekR: Vector[Generator] = {
	/** This is a specific ordering of all the genrators and all their inverses, which
		*	helps in calculating the winding number of a word.
		*/
		if (g < 1) Vector()
		else OrientableSurface(g-1).greekR ++ Vector(a(g),Inverse(b(g)),Inverse(a(g)),b(g))
	}

	def countRespectingR(xs: Vector[Generator]) : Int = {
	/** This returns all those elements which respets greekR i.e. which satisfy respectsR.
		*/
		val vecOfIndices = (0 to (xs.length-2)).toVector
		def respectsR(m: Int) : Boolean = xs(m) match {
		/** This checks if 'm'th element of xs repsects greekR i.e. checks if the inverse of
			* 'm'th element comes before 'm+1'th element in greekR.
			*/
			case Inverse(l) => greekR.indexOf(l) < greekR.indexOf(xs(m+1))
			case l => greekR.indexOf(Inverse(l)) < greekR.indexOf(xs(m+1))
		}
		if (xs.length < 2) 0
		else vecOfIndices.count(respectsR)
	}

	def windingT(w: Word) = {
	/** This returns number of elements in xs repecting greekR
		* minus number of Inverse(a(_)) present in xs
		*	minus number of b(_) present in xs
		*/
		if (w.xs.length < 2) 0
		else countRespectingR(w.xs)
				-	w.xs.count((x:Generator) => vecInvA.contains(x))
				- w.xs.count((x:Generator) => vecB.contains(x))
	}


	def isSimple(w: Word): Boolean = {
	/** This checks if all the possible divisions of w, u and v satisfy the following equation,
		* windingT(u++inv(v)) - windingT(u) - windingT(inverse(v)) = 0
		* by calling satisfyEquation(0,0)
		*
		* for details over divisions of a word, see the Reference
		*/
		val reduced = reduce(w).xs

		def satisfyEquation(i:Int,j:Int): Boolean = {
		/** Idea: Given of a permutation of a word and its division u and v,
			* one can calculate respective windingT.
			*	'i' being a index in w++w defines a permutation of w. 0 <= i <= l/2
			* 'j' is the last index of u and v is therefore the remaing elements. 0 <= j <= l-2
			*
			* This iterartes over every possible divisions of w and returns true if equation is satisfied by all.
			*/
			val l = reduced.length
			val permutated = (reduced++reduced).slice(i,i+l)
			val u = permutated.slice(0,j+1)
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
