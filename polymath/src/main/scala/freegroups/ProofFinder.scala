package freegroups

import monix.eval._, monix.tail._, cats.implicits._

// import scala.collection.mutable.{Map => mMap}

import monix.execution.Scheduler.Implicits.global

import scala.concurrent._

import LinearNorm._, LinearNormProofs._
import LinNormBound._

object ProofScript extends App {
  def run() = main(Array())

  var working = true
  import ProofFinder._
  val t20 = egTask(20)
  val f20 = t20.runToFuture
  f20.foreach { norms =>
    val normData = NormData(memMap, norms)
    val proofOpt = quickProof(Word("aba!b!"), normData)
    proofOpt.foreach { proof =>
      Console.err.println("\n")
      println("## Proof output\n")
      println(proofOut(proof).mkString("* ", "\n* ", "\n"))
      println("\n## Proof output with rational coefficients \n")
      println(RationalProofs.proofOut(proof).mkString("* ", "\n* ", "\n"))
      working = false
    }
  }
  while (working) {
    Thread.sleep(1)
  }
  working = true
}

case class PowerMove(
    word: Word,
    exp: Int,
    normBeforeOpt: Option[Double],
    normAfter: Double
)

object PowerMove {
  def historyMap(m: Map[Word, Double], moves: Vector[PowerMove]) = {
    val moveMap =
      moves.groupBy(_.word).view.mapValues(v => v.map(_.normBeforeOpt).flatten.min).toMap
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
  lazy val getProofFuture = {
    val t20 = egTask(20)
    val f20 = t20.runToFuture
    f20.map { norms =>
      val normData = NormData(memMap, norms)
      quickProof(Word("aba!b!"), normData).getOrElse(LinNormBound.Empty)
    }
  }

  lazy val getProofOutputFuture =
    getProofFuture.map { proof =>
      s"""
        |## Proof output 
        |
        |${proofOut(proof).mkString("* ", "\n* ", "\n")}
        |
        |## Proof output with rational coefficients 
        |
        |${RationalProofs.proofOut(proof).mkString("* ", "\n* ", "\n")}
        |""".stripMargin
    }

  def memScaledNorm(word: Word, n: Int) =
    for {
      t1 <- Task {
        Console.err.println(
          s"Refined using homogeneity; word: $word, exponent: $n"
        ); memoNorm.get(word.ls)
      }
      t2 <- scaledNorm(word.ls, n)
    } yield PowerMove(word, n, t1, t2)

  def homogeneityTask(seq: Vector[(Word, Int)]) =
    Task.sequence(
      seq.map {
        case (w, n) => memScaledNorm(w, n)
      }
    )

  implicit val ec = ExecutionContext.global

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
            Console.err.println(
              s"Wrong triangle inequality with ${Word(ys)} and $w"
            )
          val matchedIndices = ys.zipWithIndex.filter(_._1 == -x).map(_._2)
          val afterSplits = matchedIndices.map((i) => ys.splitAt(i)).map {
            case (a, b) => (a, b.tail)
          }
          val matchedNorms = afterSplits.filter {
            case (a, b) =>
              if (normData.norms(Word(a)) + normData.norms(Word(b)) < normData
                    .norms(w))
                Console.err.println(
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
            pf <- quickProof(w.fastPow(n), normData.takeTill(w, n))
          } yield PowerBound(w, n, pf)
        }
    }

  def getProof(w: Word): LinNormBound =
    LinearNormProofs.normProofTask(w, true).runSyncUnsafe()
}
