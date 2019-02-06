package freegroups

import Word._


object Word {

  /**
    * String for a letter, e.g. a, a! (for a inverse)
    */
  def letterString(n: Int) =
    if (n > 0) ('a' + n - 1).toChar.toString + "."
    else ('a' - n - 1).toChar.toString + "!."

  /**
    * unicode string for a letter, e.g. "a" or "\bar{a}"
    */
  def letterUnic(n: Int) =
    if (n > 0) ('a' + n - 1).toChar.toString
    else ('a' - n - 1).toChar.toString + '\u0305'.toString

  /**
    * sanity checker for VectorFromChars.
    * to add further checks later.
    */
  def isParsable(s: Vector[Char]): Boolean = {
    if (s.isEmpty) true
    else if ((s.head == '\u0305') || (s.head == '!')) false
    else true
  }

  /**
    * helper for fromString
    */
  def vectorFromChars(s: Vector[Char]): Vector[Int] = {
    require(
      isParsable(s),
      "The Vector of characters is not well formed and should not be parsed.")
    s match {
      case Vector() => Vector()
      case x +: '\u0305' +: tail =>
        (-(x - 'a' + 1)) +: vectorFromChars(tail)
      case x +: '!' +: tail =>
        (-(x - 'a' + 1)) +: vectorFromChars(tail)
      case x +: tail =>
        (x - 'a' + 1) +: vectorFromChars(tail)
    }
  }

  /**
    * word from a string.
    */
  def fromString(s: String): Word =
    if (s == "1") Word(Vector())
    else
      Word(
        vectorFromChars(
          s.replace("!", "\u0305")
            .replace(" ", "")
            .replace(".", "")
            .toVector))

  /**
    * word from a string.
    */
  def apply(w: String) = fromString(w)

  /**
    * the identity
    */
  val e = Word(Vector())

}

/**
  * A word in a free group.
  * @param ls letters of the words represented as integers; 1 represents a, -1 represents a^{-1}
  */
case class Word(ls: Vector[Int]) extends AnyVal {

  /**
    * returns reduced form of a word
    */
  def reduce: Word = {
    ls match {
      case x +: y +: zs if x == -y => Word(zs).reduce
      case x +: ys =>
        if (Word(ys).isReduced) x +: Word(ys).reduce
        else (x +: Word(ys).reduce).reduce
      case _ => this
    }
  }

  def isReduced = (this == reduce)

  /**
    * string representation
    */
  def toPlainString =
    ((ls map (letterString(_))).foldLeft("")(_ + _)).dropRight(1)

  override def toString = if (ls.isEmpty) "1" else toUnicode

  def ++(that: Word) = Word(ls ++ that.ls)

  /**
    * unicode representation.
    */
  def toUnicode = ((ls map (letterUnic(_))).foldLeft("")(_ + _))

  /**
    * letter prepended to word
    */
  def +:(let: Int) = Word(let +: ls)

  def :+(let: Int) = Word(ls :+ let)

  /**
    * inverse
    */
  def inv = Word(ls.reverse map ((n) => -n))

  /**
    * inverse
    */
  def ! = inv

  /**
    * returns this to kth power.
    */
  def pow: Int => Word = {
    case 0          => Word(Vector())
    case k if k > 0 => Word(Vector.fill(k)(ls).flatten).reduce
    case k if k < 0 => this.inv.pow(-k)
  }

  def fastPow: Int => Word = {
    case 0          => Word(Vector())
    case k if k > 0 => Word(Vector.fill(k)(ls).flatten)
    case k if k < 0 => this.inv.pow(-k)
  }

  /**
    * raise to nth power.
    */
  def ^(n: Int) = pow(n)

  /**
    * multiply and reduce
    */
  def *(that: Word) = Word(ls ++ that.ls).reduce

  /**
    * conjugate
    */
  def conj(that: Word) = that.inv * this * that

  /**
    * conjugate
    */
  def ^(that: Word) = conj(that)

  /**
    * conjugate by a generator (or its inverse)
    */
  def conjGen(k: Int) = Word((-k) +: (ls :+ k)).reduce

  /**
    * conjugate by a generator (or its inverse).
    * @param k index of generator, starting at 1.
    */
  def ^^(k: Int) = conjGen(k)

  /**
    * largest generator in the free group.
    */
  def maxgen: Int = {
    if (ls.isEmpty) 0
    else (ls map ((x: Int) => x.abs)).max
  }

  /**
    * remove generators of rank and above.
    */
  def rmvtop(rank: Int) = Word(ls filter (_.abs < rank))
}
