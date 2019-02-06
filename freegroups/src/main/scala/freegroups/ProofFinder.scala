package freegroups

import monix.eval._, monix.tail._, cats.implicits._

import scala.collection.mutable.{Map => mMap}

import monix.execution.Scheduler.Implicits.global

import LinearNorm._, LinearNormProofs.{c, cna}
import LinNormBound._

object ProofFinder {
  def memScaledNorm(word: Word, n: Int) =
    for {
      t1 <- Task {
        println(s"word: $word, exponent: $n"); memoNorm.get(word.ls)
      }
      t2 <- scaledNorm(word.ls, n)
    } yield (word, n, t1, t2)

  def homogeneityTask(seq: Vector[(Word, Int)]) =
    Task.sequence(
      seq.map {
        case (w, n) => memScaledNorm(w, n)
      }
    )

  def makeSeq(n: Int, target: Word, preCalcs: Word*): Vector[(Word, Int)] =
    preCalcs.toVector.flatMap(w => (1 to n).toVector.map(j => w -> j)) ++
      (1 to n).toVector.map(j => target -> j)

  def egSeq(n: Int) = makeSeq(n, c, cna(1), cna(2), cna(6))

  def egTask(n: Int) = homogeneityTask(egSeq(n))

  def memMap : Map[Word, Double] =
    memoNorm.map { case (v, x) => Word(v) -> x }.toMap ++ Map(
      Word("a") -> 1.0,
      Word("a!") -> 1.0,
      Word("b") -> 1.0,
      Word("b!") -> 1.0,
      Word(Vector()) -> 0.0
    )

  def quickProof(
      w: Word,
      powers: Vector[(Word, Int)],
      m: Map[Word, Double]
  ): Option[LinNormBound] =
    w.ls match {
      case Vector() => Some(Empty)
      case x +: Vector() =>
        Some(Gen(x))
      case x +: ys =>
        if (m(w) == 1 + m(Word(ys))) quickProof(Word(ys), powers, m).map { pf =>
          Triang(Gen(x), pf)
        } else {
          val matchedIndices = ys.zipWithIndex.filter(_._1 == -x).map(_._2)
          val afterSplits = matchedIndices.map((i) => ys.splitAt(i)).map {
            case (a, b) => (a, b.tail)
          }
          val matchedNorms = afterSplits.filter {
            case (a, b) =>
              println(Word(a))
              println(Word(b))
              m(Word(a)) + m(Word(b)) == m(w)
          }
          matchedNorms.headOption.flatMap {
            case (a, b) =>
              for {
                pfA <- quickProof(Word(a), powers, m)
                pfB <- quickProof(Word(b), powers, m)
              } yield Triang(ConjGen(x, pfA), pfB)
          }
        }
      // .orElse{
      //     val exps = powers.filter(_._1 == w).map(_._2)
      //     val nOpt = exps.find(n => m(w.fastPow(n))/n ==m(w))
      //     for {
      //         n <- nOpt
      //         pf <- quickProof(w, powers, m)
      //     } yield PowerBound(w, n, pf)
      // }
    }
}
