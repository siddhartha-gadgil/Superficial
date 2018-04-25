package freegroups

import monix.eval._, monix.tail._, cats.implicits._

import scala.collection.mutable.{Map => mMap}

import monix.execution.Scheduler.Implicits.global

import LinNormBound._

import annotation.tailrec

object LinearNormProofs{
  val memoNormProof: mMap[Word, LinNormBound] = mMap()

  def justSave(w: Word, pf: LinNormBound) =
    {
      memoNormProof += (w -> pf)
    }

  def save(w: Word, pf: LinNormBound, noSym: Boolean) =
    if (noSym) justSave(w, pf)
    else symmProofs.map(_(pf)).foreach(savePf)

  def savePf(pf: LinNormBound) = justSave(pf.word, pf)

  def update(pf: LinNormBound, noSym: Boolean) =
    memoNormProof.get(pf.word).foreach{
      (mpf) => if (mpf.bound > pf.bound) save(pf.word, pf, noSym)
    }

  def updated(pf: LinNormBound, noSym: Boolean) =
    {
      update(pf, noSym)
      pf
    }

  def memOnly(ws: Word*) = {
    val mem = ws.toVector.map((w) => (w, memoNormProof(w)))
    memoNormProof.clear
    mem.foreach {case (w, pf) => save(w, pf, false)}
  }

  def minWithMemo(pf: LinNormBound, noSym: Boolean) =
    memoNormProof.get(pf.word).map{
      (mpf) =>
        if (mpf.bound < pf.bound) mpf
        else updated(pf, noSym)
    }.getOrElse(pf)

  def tighten(pf: LinNormBound, noSym: Boolean = true) : LinNormBound = pf match {
    case ConjGen(n, pf) => minWithMemo(ConjGen(n, tighten(pf)), noSym)
    case Triang(a, b) => minWithMemo(tighten(a) ++ tighten(b), noSym)
    case PowerBound(baseword, n, pf) => minWithMemo(PowerBound(baseword, n, tighten(pf)), noSym)
    case p => minWithMemo(p, noSym)
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
    case ConjGen(n, pf) => proofLines(pf) :+ leqUse(ConjGen(-n, pf), pf)
    case Triang(a, b) => (proofLines(a) ++ proofLines(b)) :+ leqUse(a ++ b, a, b)
    case PowerBound(baseword, n, pf) =>
      proofLines(pf) :+ (leqUse(PowerBound(baseword, n, pf)) + s" by taking ${n}th power")
    case Empty => Vector()
  }

  def proofOut(pf: LinNormBound) = proofLines(pf).distinct

  @tailrec def proofPowers(pf: LinNormBound, n: Int, accum: Vector[LinNormBound] = Vector()): Vector[LinNormBound] =
    if (n <= accum.size) accum.take(n)
    else if (accum.isEmpty) Vector(pf)
    else proofPowers(pf, n, accum :+ (accum.last ++ pf))

  def updateInverses() =
    for {
      (w, pf) <- memoNormProof
      invPf <- memoNormProof.get(w.inv)
    } if (invPf.bound < pf.bound) memoNormProof += (w -> LinNormBound.inverse(invPf))

  def updateSymm() =
    for {
      (w, pf) <- memoNormProof
      sy <- symmProofs
      spf = sy(pf)
      oldpf <- memoNormProof.get(spf.word)
    } if (oldpf.bound > spf.bound) savePf(spf)

  def propagate(pf: LinNormBound) =
    {
      val gains =
        memoNormProof.filter{
          case (w, p) =>
            (memoNormProof.get(w ++ pf.word).map(_.bound).getOrElse(0.0) > (pf.bound + p.bound))
        }.map {case (w, p) => (w ++ pf.word, p ++ pf)}
      gains.foreach{
        case (w, p) => save(w, p, false)
      }
      gains.toVector.distinct
    }

  def propagateAll(pfs: Vector[LinNormBound]) = pfs.flatMap(propagate).distinct

  // Better to propagate
  def updateTriang() =
    for {
      (w1, pf1) <- memoNormProof
      (w2, pf2) <- memoNormProof
      sumPf <- memoNormProof.get(w1 ++ w2)
    } if (sumPf.bound > pf1.bound + pf2.bound)
        memoNormProof += ((w1 ++ w2) -> (pf1 ++ pf2))

  import LinNormBound._

  def normProofTask(word: Word, noSym: Boolean): Task[LinNormBound] =
    memoNormProof.get(word).map(Task(_)).
    getOrElse{
    Task(word.ls). flatMap {
      case Vector() => Task.pure(Empty)
      case x +: Vector() => Task.pure(Gen(x))
      case x +: ys =>
        if (x == - ys.last) normProofTask(Word(ys.init), noSym).map((pf) => x *: pf)
        else
          {
            val matchedIndices = ys.zipWithIndex.filter(_._1 == -x).map(_._2)
            val afterSplits = matchedIndices.map((i) => ys.splitAt(i)).map{case (a, b) => (a, b.tail)}
            val recNormsTask = Task.gather(
              afterSplits.map{
                case (ta, tb) =>
                  for {
                    a <-normProofTask(Word(ta), noSym)
                    b <- normProofTask(Word(tb), noSym)
                  } yield  (x *: a)  ++ b
                }
                )
            for {
              recNorms <- recNormsTask
              ynorm <- normProofTask(Word(ys), noSym)
              res = ((x +: ynorm) +: recNorms).minBy(_.bound)
              _ = save(word, res, noSym)
            } yield res
        }
      }
  }

  def scaledNormProof(word: Word, n: Int, noSym: Boolean) =
    normProofTask(word.pow(n), noSym).map{(x) =>
      val res = PowerBound(word, n, x)
      memoNormProof.get(word).foreach{
        (p) => if (p.bound > res.bound) save(word, res, noSym)
      }
      res}

  def scaledTaskProofs(word: Word, start: Int, stop: Int, noSym: Boolean) = {
    val it = Iterant.range[Task](start, stop).scanEval[Vector[LinNormBound]](Task.pure(Vector())){
        case (v, n) =>
        for {
          res <- scaledNormProof(word, n, noSym)
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


    val c = Word("aba!b!")
    def cna(n: Int) = c.pow(n) ++ Word("a")
    def cnab(n: Int) = c.pow(n) ++ Word("ab")
    def cnB(n: Int) = Word("b!") ++ c.pow(n)

    def cpf = memoNormProof(c)
    def cbound = cpf.bound

    def fc(n: Int = 10, noSym: Boolean = true) = scaledTaskProofs(c, 1, n, noSym).runAsync
    def fcna(k: Int, n: Int = 10, noSym: Boolean = true) = scaledTaskProofs(cna(k), 1, n, noSym).runAsync
    def fcnab(k: Int, n: Int = 10, noSym: Boolean = true) = scaledTaskProofs(cnab(k), 1, n, noSym).runAsync
    def fcnB(k: Int, n: Int = 10, noSym: Boolean = true) = scaledTaskProofs(cnB(k), 1, n, noSym).runAsync


}
