package freegroups

import monix.eval._, monix.tail._, cats.implicits._

import scala.collection.mutable.{Map => mMap}

import monix.execution.Scheduler.Implicits.global

import scala.util.Random

object LinearNorm {
  val memoNorm: mMap[Vector[Int], Double] = mMap()

  val lengthsMap : mMap[Int, Vector[Double]] = mMap()

  val lengthsSumCount : mMap[Int, (Double, Int)] = mMap()
  
  def average(v: Vector[Double]) = v.sum/v.size

  val  averages : mMap[Int, (Double, Int)] = mMap()

  val memoMaxLength = 100

  def update(word: Vector[Int], res: Double) = {
    val size = word.length
    if (size <= memoMaxLength) memoNorm += word -> res
    // val newVec = lengthsMap.getOrElse(word.size, Vector.empty[Double]) :+ res
    // lengthsMap(word.size) = newVec
    val (totalLength, count) = lengthsSumCount.getOrElse(size, (0.0, 0))
    lengthsSumCount(size) = (totalLength + res, count + 1)
    averages(size) = (totalLength + res)/(count + 1) -> (count + 1)
  }

  val rnd = new Random()

  def randomLetter : Int = {
    val n = rnd.nextInt(4) - 1
    if (n > 0) n else n - 1
  }

  def letter(m: Int) = {
    val n = m - 1
    if (n > 0) n else n - 1
  }

  def allWords(n: Int) : Vector[Vector[Int]] =  
  if (n == 0) Vector(Vector())
  else {
    val tail = allWords(n - 1)
    for {
      i<- (0 to 3).toVector
      l = letter(i)
      t <- tail 
    } yield l +: t
  }

  def wordsUpto(n: Int) : Vector[Vector[Int]] = (0 to n).toVector.flatMap(allWords)

  def power(v: Vector[Int], n: Int) : Vector[Int] = 
    if (n == 0) Vector()
    else v ++ power(v, n-1)

  def normRatioTask(w: Vector[Int], n: Int) = 
    for {
      x <- normTask(w)
      y <- normTask(power(w, n))
    } yield (x * n)/ y

  def aFamily(w: Vector[Int], exps : Vector[Int]) = 
   exps.map(n => 1 +: power(w, n))

  def aFamilyRatios(bound: Int, exps : Vector[Int], pows : Vector[Int]) =
    {
      val ws = wordsUpto(bound).flatMap(w => aFamily(w, exps))
      val tasks = for {
        w <- ws
        n <- pows
      } yield normRatioTask(w, n).map(r => (w, n, r))
      tasks
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
