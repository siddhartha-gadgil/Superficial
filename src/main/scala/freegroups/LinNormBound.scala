package freegroups

import LinNormBound._

sealed class LinNormBound(val word: Vector[Int], val bound: Double){
  def ++(that: LinNormBound) = Triang(word, that.word, this, that)

  def *:(n: Int) = ConjGen(n, word, this)

  def +:(n: Int) = Gen(n) ++ this
}

object LinNormBound{
  case class Gen(n: Int) extends LinNormBound(Vector(n), 1)

  case class ConjGen(n: Int, baseword: Vector[Int], pf: LinNormBound) extends LinNormBound(n +: baseword :+ (-n), pf.bound){
    assert(pf.word == baseword)
  }

  case class Triang(w1: Vector[Int], w2: Vector[Int], pf1: LinNormBound, pf2: LinNormBound) extends LinNormBound(w1 ++ w2, pf1.bound + pf2.bound){
    assert(pf1.word == w1)
    assert(pf2.word == w2)
  }

  case class PowerBound(baseword: Vector[Int], n: Int, pf: LinNormBound) extends LinNormBound(baseword, pf.bound/n){
    assert(pf.word == Vector.fill(n)(baseword).reduce(_ ++ _), s"power bound failed, ${pf.word}, $baseword, $n")
  }

  case object Empty extends LinNormBound(Vector(), 0)
}
