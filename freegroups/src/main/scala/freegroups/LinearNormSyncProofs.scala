package freegroups


import scala.collection.mutable.{Map => mMap}


import LinNormBound._, LinearNorm._

import annotation.tailrec

object LinearNormSyncProofs {


  val memoNormProof: mMap[Word, LinNormBound] = mMap()

  def justSave(w: Word, pf: LinNormBound) = {
    memoNormProof += (w -> pf)
    memoNorm += (w.ls -> pf.bound)
  }

  def save(w: Word, pf: LinNormBound, noSym: Boolean) =
    if (noSym) justSave(w, pf)
    else symmProofs.map(_(pf)).foreach(savePf)

  def savePf(pf: LinNormBound) = justSave(pf.word, pf)


  def leq(pf: LinNormBound) =
    s"|${pf.word}| \u2264 ${pf.bound}"

  def leqUse(pf: LinNormBound, used: LinNormBound*) = {
    val reasons = used.toVector.map(leq).mkString(" and ")
    s"${leq(pf)} using $reasons"
  }

  def proofLines: LinNormBound => Vector[String] = {
    case Gen(n)         => Vector(leq(Gen(n)))
    case ConjGen(n, pf) => proofLines(pf) :+ leqUse(ConjGen(-n, pf), pf)
    case Triang(a, b) =>
      (proofLines(a) ++ proofLines(b)) :+ leqUse(a ++ b, a, b)
    case PowerBound(baseword, n, pf) =>
      proofLines(pf) :+ (leqUse(PowerBound(baseword, n, pf), pf) + s" by taking ${n}th power")
    case Empty => Vector()
  }

  def proofOut(pf: LinNormBound) = proofLines(pf).distinct

  def normProof(word: Word, noSym: Boolean): LinNormBound =
    memoNormProof.get(word).getOrElse {
      (word.ls) match {
        case Vector()      => Empty
        case x +: Vector() => Gen(x)
        case x +: ys =>
          if (x == -ys.last)
            x *: normProof(Word(ys.init), noSym)
          else {
            val matchedIndices = ys.zipWithIndex.filter(_._1 == -x).map(_._2)
            val afterSplits = matchedIndices.map((i) => ys.splitAt(i)).map {
              case (a, b) => (a, b.tail)
            }
            val recNorms = (
              afterSplits.map {
                case (ta, tb) =>
                    val a = norm(ta)
                    val b = norm(tb)
                  (ta, tb, a + b)
              }
            )
            
              val ynorm = norm(ys)
              val res = ((1 + ynorm) +: recNorms.map(_._3)).min
              val resProof = 
                if (res == 1 + ynorm) x +: normProof(Word(ys), noSym) 
                else {
                  val (ta, tb, _) =recNorms.minBy(_._3) 
                  (x *: normProof(Word(ta), noSym)) ++ normProof(Word(tb), noSym)}
              save(word, resProof, noSym)
            resProof
          }
      }
    }

  def scaledNormProof(word: Word, n: Int, noSym: Boolean) =
    { val x = normProof(word.fastPow(n), noSym)
      val res = PowerBound(word, n, x)
      memoNormProof.get(word).foreach { (p) =>
        if (p.bound > res.bound) save(word, res, noSym)
      }
      res
    }

  def computeScaledNormProof(word: Word, n: Int, noSym: Boolean = true) =
    {scaledNormProof(word, n, noSym) ; ()}


  val c = Word("aba!b!")
  def cna(n: Int) = c.pow(n) ++ Word("a")
  def cnab(n: Int) = c.pow(n) ++ Word("ab")
  def cnB(n: Int) = Word("b!") ++ c.pow(n)

  def cpf = memoNormProof(c)
  def cbound = cpf.bound

}
