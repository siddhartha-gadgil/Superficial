package freegroups

import monix.eval._

object LinearNorm{
  def norm(word: Vector[Int]): Int = word match {
    case Vector() => 0
    case x +: Vector() => 1
    case x +: ys =>
        val matchedIndices = ys.zipWithIndex.filter(_._1 == -x).map(_._2)
        val afterSplits = matchedIndices.map((i) => ys.splitAt(i)).map{case (a, b) => (a, b.tail)}
        val recNorms = afterSplits.map{case (a, b) => norm(a) + norm(b)}
        ((1 + norm(ys)) +: recNorms).min
  }

  def normTask(word: Vector[Int]): Task[Int] =
    Task(word). flatMap {
      case Vector() => Task.pure(0)
      case x +: Vector() => Task.pure(1)
      case x +: ys =>
          val matchedIndices = ys.zipWithIndex.filter(_._1 == -x).map(_._2)
          val afterSplits = matchedIndices.map((i) => ys.splitAt(i)).map{case (a, b) => (a, b.tail)}
          val recNormsTask = Task.gather(
            afterSplits.map{
              case (ta, tb) =>
                for {
                  a <-normTask(ta)
                  b <- normTask(tb)
                } yield a + b
              }
              )
          for {
            recNorms <- recNormsTask
            ynorm <- normTask(ys)
          } yield ((1 + ynorm) +: recNorms).min
  }

}
