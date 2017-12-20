package freegroups

import monix.eval._, monix.tail._, cats.implicits._

import scala.collection.mutable.{Map => mMap}

import monix.execution.Scheduler.Implicits.global


object LinearNormProofs{
  val memoNormProof: mMap[Word, LinNormBound] = mMap()

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
          _ = println(res)
         } yield v :+ res
       }

    val task = it.foldL
    task

  }

}
