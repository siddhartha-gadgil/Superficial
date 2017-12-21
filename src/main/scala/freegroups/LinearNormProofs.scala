package freegroups

import monix.eval._, monix.tail._, cats.implicits._

import scala.collection.mutable.{Map => mMap}

import monix.execution.Scheduler.Implicits.global

import LinNormBound._

import annotation.tailrec

object LinearNormProofs{
  val memoNormProof: mMap[Word, LinNormBound] = mMap()

  def update(pf: LinNormBound) =
    memoNormProof.get(pf.word).foreach{
      (mpf) => if (mpf.bound > pf.bound) memoNormProof += (pf.word -> pf)
    }

  def updated(pf: LinNormBound) =
    {
      update(pf)
      pf
    }

  def memOnly(ws: Word*) = {
    val mem = ws.toVector.map((w) => (w, memoNormProof(w)))
    memoNormProof.clear
    memoNormProof ++= mem
  }

  def minWithMemo(pf: LinNormBound) =
    memoNormProof.get(pf.word).map{
      (mpf) =>
        if (mpf.bound < pf.bound) mpf
        else updated(pf)
    }.getOrElse(pf)

  def tighten(pf: LinNormBound) : LinNormBound = pf match {
    case ConjGen(n, pf) => minWithMemo(ConjGen(n, tighten(pf)))
    case Triang(a, b) => minWithMemo(tighten(a) ++ tighten(b))
    case PowerBound(baseword, n, pf) => minWithMemo(PowerBound(baseword, n, tighten(pf)))
    case p => minWithMemo(p)
  }

  def leq(pf: LinNormBound) =
    s"|${pf.word}| \u2264 ${pf.bound}"

  def leqUse(pf: LinNormBound, used: LinNormBound*) =
    {
      val reasons = used.toVector.map(leq).mkString(" and ")
      s"${leq(pf)} using $reasons"
    }

  def proofLines : LinNormBound => Vector[String] = {
    case Gen(n) => Vector(leq(Gen(n)))
    case ConjGen(n, pf) => leqUse(ConjGen(-n, pf), pf) +: proofLines(pf)
    case Triang(a, b) => leqUse(a ++ b, a, b) +: (proofLines(a) ++ proofLines(b))
    case PowerBound(baseword, n, pf) =>
      (leqUse(PowerBound(baseword, n, pf)) + s" by taking ${n}th power") +: proofLines(pf)
    case Empty => Vector()
  }

  def proofOut(pf: LinNormBound) = proofLines(pf).distinct.reverse

  @tailrec def proofPowers(pf: LinNormBound, n: Int, accum: Vector[LinNormBound] = Vector()): Vector[LinNormBound] =
    if (n <= accum.size) accum.take(n)
    else if (accum.isEmpty) Vector(pf)
    else proofPowers(pf, n, accum :+ (accum.last ++ pf))

  def updateInverses() =
    for {
      (w, pf) <- memoNormProof
      invPf <- memoNormProof.get(w.inv)
    } if (invPf.bound < pf.bound) memoNormProof += (w -> LinNormBound.inverse(invPf))

  def updateTriang() =
    for {
      (w1, pf1) <- memoNormProof
      (w2, pf2) <- memoNormProof
      sumPf <- memoNormProof.get(w1 ++ w2)
    } if (sumPf.bound > pf1.bound + pf2.bound)
        memoNormProof += ((w1 ++ w2) -> (pf1 ++ pf2))

  import LinNormBound._

  def normProofTask(word: Word): Task[LinNormBound] =
    memoNormProof.get(word).map(Task(_)).
    getOrElse{
    Task(word.ls). flatMap {
      case Vector() => Task.pure(Empty)
      case x +: Vector() => Task.pure(Gen(x))
      case x +: ys =>
        if (x == - ys.last) normProofTask(Word(ys.init)).map((pf) => x *: pf)
        else
          {
            val matchedIndices = ys.zipWithIndex.filter(_._1 == -x).map(_._2)
            val afterSplits = matchedIndices.map((i) => ys.splitAt(i)).map{case (a, b) => (a, b.tail)}
            val recNormsTask = Task.gather(
              afterSplits.map{
                case (ta, tb) =>
                  for {
                    a <-normProofTask(Word(ta))
                    b <- normProofTask(Word(tb))
                  } yield  (x *: a)  ++ b
                }
                )
            for {
              recNorms <- recNormsTask
              ynorm <- normProofTask(Word(ys))
              res = ((x +: ynorm) +: recNorms).minBy(_.bound)
              _ = memoNormProof += word -> res
            } yield res
        }
      }
  }

  def scaledNormProof(word: Word, n: Int) =
    normProofTask(word.pow(n)).map{(x) =>
      val res = PowerBound(word, n, x)
      memoNormProof.get(word).foreach{
        (p) => if (p.bound > res.bound) memoNormProof += (word -> res)
      }
      res}

  def scaledTaskProofs(word: Word, start: Int, stop: Int) = {
    val it = Iterant.range[Task](start, stop).scanEval[Vector[LinNormBound]](Task.pure(Vector())){
        case (v, n) =>
        for {
          res <- scaledNormProof(word, n)
         } yield v :+ res
       }

    val task =
      for {
        vec <- it.foldL
        pf = vec.minBy(_.bound)
        _ = println(s"obtained bound ${pf.bound} for ${pf.word}")
        // pfPows = proofPowers(pf, stop)
        // _ = pfPows.foreach(update)
      } yield pf
    task

  }

}
