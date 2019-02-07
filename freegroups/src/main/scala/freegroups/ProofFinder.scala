package freegroups

import monix.eval._, monix.tail._, cats.implicits._

import scala.collection.mutable.{Map => mMap}

import monix.execution.Scheduler.Implicits.global

import LinearNorm._, LinearNormProofs._
import LinNormBound._

case class PowerMove(
    word: Word,
    exp: Int,
    normBeforeOpt: Option[Double],
    normAfter: Double
)

object PowerMove {
  def historyMap(m: Map[Word, Double], moves: Vector[PowerMove]) = {
    val moveMap =
      moves.groupBy(_.word).mapValues(v => v.map(_.normBeforeOpt).flatten.min)
    m ++ moveMap
  }
}

case class NormData(norms: Map[Word, Double], moves: Vector[PowerMove]) {
  def takeTill(word: Word, n: Int) = {
    val head = moves.takeWhile(m => (m.word, m.exp) != (word, n))
    val tail = moves.drop(head.size)
    NormData(PowerMove.historyMap(norms, tail), head)
  }

  def wordNormOpt(word: Word, norm: Double): Option[(Word, NormData)] =
    if (norms(word) == norm) Some(word -> this) else None
}

object ProofFinder {
  def memScaledNorm(word: Word, n: Int) =
    for {
      t1 <- Task {
        println(s"word: $word, exponent: $n"); memoNorm.get(word.ls)
      }
      t2 <- scaledNorm(word.ls, n)
    } yield PowerMove(word, n, t1, t2)

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

  def memMap: Map[Word, Double] =
    memoNorm.map { case (v, x) => Word(v) -> x }.toMap ++ Map(
      Word("a") -> 1.0,
      Word("a!") -> 1.0,
      Word("b") -> 1.0,
      Word("b!") -> 1.0,
      Word(Vector()) -> 0.0
    )

  def quickProof(
      w: Word,
      normData: NormData
  ): Option[LinNormBound] =
    w.ls match {
      case Vector() => Some(Empty)
      case x +: Vector() =>
        Some(Gen(x))
      case x +: ys =>
        if (normData.norms(w) == 1 + normData.norms(Word(ys)))
          quickProof(Word(ys), normData).map { pf =>
            Triang(Gen(x), pf)
          } else {
          if (normData.norms(w) > 1 + normData.norms(Word(ys)))
            println(s"Wrong triangle inequality with ${Word(ys)} and $w")
          val matchedIndices = ys.zipWithIndex.filter(_._1 == -x).map(_._2)
          val afterSplits = matchedIndices.map((i) => ys.splitAt(i)).map {
            case (a, b) => (a, b.tail)
          }
          val matchedNorms = afterSplits.filter {
            case (a, b) =>
              if (normData.norms(Word(a)) + normData.norms(Word(b)) < normData
                    .norms(w))
                println(
                  s"Wrong triangle inequality with ${Word(a)}, ${Word(b)} and $w"
                )
              normData.norms(Word(a)) + normData.norms(Word(b)) == normData
                .norms(w)
          }
          matchedNorms.headOption.flatMap {
            case (a, b) =>
              for {
                pfA <- quickProof(Word(a), normData)
                pfB <- quickProof(Word(b), normData)
              } yield Triang(ConjGen(x, pfA), pfB)
          }
        }.orElse {
            val exps =
              normData.moves.filter(_.word == w).map(_.exp).filter(_ > 1)
            val nOpt = exps.find(
              n => normData.norms(w.fastPow(n)) / n == normData.norms(w)
            )
            for {
              n <- nOpt
              _ = println(n)
              _ = println(w)
              pf <- quickProof(w.fastPow(n), normData.takeTill(w, n))
            } yield PowerBound(w, n, pf)
          }
    }
}
