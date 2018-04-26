package freegroups

import LinNormBound._

sealed abstract class LinNormBound(val word: Word, val bound: Double) {
  def ++(that: LinNormBound) = Triang(this, that)
  def *:(n: Int) = ConjGen(n, this)
  def +:(n: Int) = Gen(n) ++ this
}

object LinNormBound{
  final case class Gen(n: Int) extends LinNormBound(Word(Vector(n)), 1){
    require(n != 0, "No generator with index 0")
    override val toString = Word(Vector(n)).toString
  }

  final case class ConjGen(n: Int,pf: LinNormBound) extends LinNormBound(n +: pf.word :+ (-n), pf.bound){
    require(n != 0, "No generator with index 0")
  }

  final case class Triang(pf1: LinNormBound, pf2: LinNormBound) extends LinNormBound(pf1.word ++ pf2.word, pf1.bound + pf2.bound)

  final case class PowerBound(baseword: Word, n: Int, pf: LinNormBound) extends LinNormBound(baseword, pf.bound/n){
    require(pf.word == baseword.pow(n), s"The element ${pf.word} is not the ${n}th power of $baseword")
  }

  final case object Empty extends LinNormBound(Word(Vector()), 0)

  def inverse: LinNormBound => LinNormBound = {
    case Gen(n) => Gen(-n)
    case ConjGen(n, pf) => ConjGen(n, inverse(pf))
    case Triang(a, b) => Triang(inverse(b), inverse(a))
    case PowerBound(baseword, n, pf) =>
      PowerBound(baseword.inv, n, inverse(pf))
    case Empty => Empty
  }

  def subProofs: LinNormBound => Set[LinNormBound] = {
    case Gen(n) => Set(Gen(n))
    case ConjGen(n, pf) => subProofs(pf) + ConjGen(-n, pf)
    case Triang(a, b) => (subProofs(a) union subProofs(b)) + Triang(a, b)
    case PowerBound(baseword, n, pf) =>
      subProofs(pf) + PowerBound(baseword, n, pf)
    case Empty => Set(Empty)
  }

  def symm(f: Int => Int): LinNormBound => LinNormBound = {
    case Gen(n) => Gen(f(n))
    case ConjGen(n, pf) => ConjGen(f(n), symm(f)(pf))
    case Triang(a, b) => Triang(symm(f)(a), symm(f)(b))
    case PowerBound(baseword, n, pf) =>
      PowerBound(Word(baseword.ls.map(f)), n, symm(f)(pf))
    case Empty => Empty
  }

  def flip: Int => Int = (x) => (-x)
  def flipOdd(n: Int) = if (math.abs(n) % 2 == 1) -n else n
  def flipEven(n: Int) = if (n % 2 == 0) -n else n
  val id = identity[Int](_)

  val symmGens: Vector[Int => Int] = Vector(id, flip, flipOdd, flipEven)

  val symmProofs: Vector[LinNormBound => LinNormBound] =
    symmGens.flatMap((f) =>
      Vector(symm(f), (w: LinNormBound) => symm(f)(inverse(w))))
}
