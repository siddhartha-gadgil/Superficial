package freegroups
import monix.eval._

import ProofFinder.{rnd, randomWord}, LinearNorm.normTask
import monix.execution.Scheduler.Implicits.global

object HybridAlgos {
  def matchedIndices(w: Word) =
    for {
      i <- 0 until w.ls.size - 1
      j <- i + 1 until w.ls.size
      if (w.ls(i) == -w.ls(j))
    } yield (i, j)

  def symmDist(i: Int, j: Int, n: Int) =
    math.min(j - i, n - j + i)

  def recNorm(
      w: Word,
      threshold: Int,
      maximize: Boolean = true
  ): Task[Double] = {
    val n = w.ls.size
    val pairs = matchedIndices(w)
    if (pairs.isEmpty) {
      // println(w)
      // println(w.ls)
      Task(n)
    } else if (w.ls.size < threshold) normTask(w.ls)
    else {
      val (i, j) = if (maximize) pairs.maxBy {
        case (i, j)                    => symmDist(i, j, n)
      } else pairs.minBy { case (i, j) => symmDist(i, j, n) }
      val head = Word(w.ls.take(i))
      val tail = Word(w.ls.drop(j + 1))
      val mid = Word(w.ls.take(j).drop(i + 1))
      // println(s"$w becomes $head, $mid, $tail split at ($i : ${Word(Vector(w.ls(i)))}, $j : ${Word(Vector(w.ls(j)))})")
      Task.parMap3(
        recNorm(head, threshold, maximize),
        recNorm(tail, threshold, maximize),
        recNorm(mid, threshold, maximize)
      ) { case (x, y, z) => x + y + z }
    }
  }

  @annotation.tailrec
  def choose[A](s: Set[A], n: Int, accum: Set[A]): Set[A] = {
    if (n < 1 || s.isEmpty) accum
    else {
      val choice = s.toVector(rnd.nextInt(n))
      choose(s - choice, n - 1, accum)
    }
  }

  def branchedNorm(
      w: Word,
      threshold: Int,
      depth: Int,
      extraBranches: Int
  ): Task[Double] = {
    val n = w.ls.size
    val pairs = matchedIndices(w)
    if (pairs.isEmpty) {
      Task(n)
    } else if (w.ls.size < threshold) normTask(w.ls)
    else {
      val (i0, j0) = pairs.maxBy { case (i, j) => symmDist(i, j, n) }
      val allPairs =
        if (depth > 0) choose(pairs.toSet, extraBranches, Set()) + ((i0, j0))
        else Set((i0, j0))
      val bounds = allPairs.map {
        case (i, j) =>
          val head = Word(w.ls.take(i))
          val tail = Word(w.ls.drop(j + 1))
          val mid = Word(w.ls.take(j).drop(i + 1))
          // println(s"$w becomes $head, $mid, $tail split at ($i : ${Word(Vector(w.ls(i)))}, $j : ${Word(Vector(w.ls(j)))})")
          Task.parMap3(
            branchedNorm(head, threshold, depth - 1, extraBranches),
            branchedNorm(tail, threshold, depth - 1, extraBranches),
            branchedNorm(mid, threshold, depth - 1, extraBranches)
          ) { case (x, y, z) => x + y + z }
      }
      Task.gather(bounds).map(_.min)
    }
  }

  def averageActual(length: Int, sampleSize: Int): Task[Double] = {
    val ws = (1 to sampleSize).map(_ => ProofFinder.randomWord(length).reduce)
    val total = Task.gather(ws.map(w => normTask(w.ls))).map(_.sum)
    total.map(t => t / (sampleSize * length))
  }

  def averageHybrid(
      length: Int,
      threshold: Int,
      sampleSize: Int,
      maximize: Boolean = false
  ): Task[(Double, Double)] = {
    val ws = (1 to sampleSize).map(_ => ProofFinder.randomWord(length).reduce)
    val values = Task.gather(ws.map(w => (recNorm(w, threshold, maximize).map(a => a/ length)))).memoize
    val total =
      values.map(_.sum)
    val totalSquare = values.map(v =>  v.map(x => x * x).sum) 
    Task.parMap2(total, totalSquare){
        case (sum, sumOfSquares) =>
            val mean = sum / sampleSize
            val variance = (sumOfSquares / sampleSize) - (mean * mean)
            (mean, math.sqrt(variance))
    }
  }

  def branchedAverage(
      length: Int,
      threshold: Int,
      sampleSize: Int,
      depth: Int,
      extraBranches: Int
  ): Task[(Double, Double)] = {
    val ws = (1 to sampleSize).map(_ => ProofFinder.randomWord(length).reduce)
    val values = Task.gather(ws.map(w => (branchedNorm(w, threshold, depth, extraBranches).map(a => a/ length)))).memoize
    val total =
      values.map(_.sum)
    val totalSquare = values.map(v =>  v.map(x => x * x).sum) 
    Task.parMap2(total, totalSquare){
        case (sum, sumOfSquares) =>
            val mean = sum / sampleSize
            val variance = (sumOfSquares / sampleSize) - (mean * mean)
            (mean, math.sqrt(variance))
    }
  }

  def averageList(step: Int, sampleSize: Int = 100) =
    LazyList.from(1).map(_ * step).map { n =>
      n -> averageActual(n, sampleSize).runToFuture
    }
}
