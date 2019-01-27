package freegroups

import monix.eval._, monix.tail._, cats.implicits._

import scala.collection.mutable.{Map => mMap}

import monix.execution.Scheduler.Implicits.global

import scala.util.Random

object LinearNorm {
  val memoNorm: mMap[Vector[Int], Double] = mMap()

  def lengthsMap : Map[Int, Vector[Double]] = memoNorm.toVector.groupBy(_._1.length).mapValues(_.map(_._2))

  def average(v: Vector[Double]) = v.sum/v.size

  def averages = lengthsMap.mapValues(average).toVector.sortBy(_._1)

  val rnd = new Random()

  def randomLetter : Int = {
    val n = rnd.nextInt(4)
    if (n > 0) n else n - 1
  }

  def randomWord(l: Int) : Vector[Int] =
    Vector.fill(l)(randomLetter)

    def getRandomNorm(n: Int) = {
      val w = randomWord(n)
      println(Word(w))
      val fut = normTask(randomWord(n)).materialize.runToFuture
      fut.foreach((normTry) => normTry.fold((err) => println(err),
        norm => println(s"length: $n -> norm: $norm")
        ))
      fut
    }

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
          if (x == -ys.last) normTask(ys.init)
          else {
            val matchedIndices = ys.zipWithIndex.filter(_._1 == -x).map(_._2)
            val afterSplits = matchedIndices.map((i) => ys.splitAt(i)).map {
              case (a, b) => (a, b.tail)
            }
            val recNormsTask = Task.gather(
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
              _ = memoNorm += word -> res
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

  def scaledTask(word: Vector[Int], start: Int, stop: Int) = {
    val it = Iterant
      .range[Task](start, stop)
      .scanEval[Vector[Double]](Task.pure(Vector())) {
        case (v, n) =>
          for {
            res <- scaledNorm(word, n)
            _ = println(res)
          } yield v :+ res
      }

    val task = it.foldL
    task

  }

}
