package freegroups

import monix.eval._, monix.tail._, cats.implicits._

import scala.collection.mutable

import monix.execution.Scheduler.Implicits.global

import scala.util.Random

object LinearNorm {
  val memoNorm: mutable.Map[Vector[Int], Double] = mutable.Map()

  var memoLimit: Option[Int] = None

  def update(word: Vector[Int], res: Double) = {
    val size = word.length
    if (memoLimit.map(b => size <= b).getOrElse(true)) memoNorm += word -> res
  }

  def clearMap(limit: Int) = memoNorm.filterInPlace{
    case (k, _) => k.size <= limit
  }


  def power(v: Vector[Int], n: Int) : Vector[Int] = 
    if (n == 0) Vector()
    else v ++ power(v, n-1)

  def norm(word: Vector[Int]): Int = word match {
    case Vector()      => 0
    case x +: Vector() => 1
    case x +: ys =>
      val matchedIndices = ys.zipWithIndex.filter(_._1 == -x).map(_._2)
      val afterSplits = matchedIndices.map((i) => ys.splitAt(i)).map {
        case (a, b) => (a, b.tail)
      }
      val recNorms = afterSplits.map { case (a, b) => norm(a) + norm(b) }
      ((1 + norm(ys)) +: recNorms).min
  }

  def normTask(word: Vector[Int]): Task[Double] =
    memoNorm.get(word).map(Task(_)).getOrElse {
      Task(word).flatMap {
        case Vector()      => Task.pure(0)
        case x +: Vector() => Task.pure(1)
        case x +: ys =>
          if (x == -ys.last) normTask(ys.init).map{
            res =>
              update(word, res)
              res
          }
          else {
            val matchedIndices = ys.zipWithIndex.filter(_._1 == -x).map(_._2)
            val afterSplits = matchedIndices.map((i) => ys.splitAt(i)).map {
              case (a, b) => (a, b.tail)
            }
            val recNormsTask = Task.sequence(
              afterSplits.map {
                case (ta, tb) =>
                  for {
                    a <- normTask(ta)
                    b <- normTask(tb)
                  } yield a + b
              }
            )
            for {
              recNorms <- recNormsTask
              ynorm <- normTask(ys)
              res = ((1 + ynorm) +: recNorms).min
              _ = update(word, res)
            } yield res
          }
      }
    }

  def scaledNorm(word: Vector[Int], n: Int) =
    normTask(Vector.fill(n)(word).reduce(_ ++ _)).map { (x) =>
      val res = x / n
      memoNorm.get(word).foreach { (p) =>
        if (p > res) memoNorm += (word -> res)
      }
      res
    }

 
}
