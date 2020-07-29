package freegroups
import monix.eval._

import ProofFinder.{rnd, randomWord}, LinearNorm.normTask
import monix.execution.Scheduler.Implicits.global

object HybridAlgos{
    def matchedIndices(w: Word) = 
        for {
            i <- 0 until w.ls.size - 1
            j <- i + 1 until w.ls.size
            if (w.ls(i) == -w.ls(j))
        } yield (i, j)
    
    def symmDist(i: Int, j: Int, n: Int) = 
        math.min(j - i, n - j + i)

    def recNorm(w: Word, threshold: Int, maximize: Boolean = true): Task[Double] = {
        val n = w.ls.size
        val pairs = matchedIndices(w)
        if (pairs.isEmpty){
            // println(w)
            // println(w.ls)
         Task(n)
        }
        else if (w.ls.size < threshold) normTask(w.ls)
        else {
            val (i, j) = if (maximize) pairs.maxBy{case (i, j) => symmDist(i, j, n)} else pairs.minBy{case (i, j) => symmDist(i, j, n)}
            val head = Word(w.ls.take(i))
            val tail = Word(w.ls.drop(j + 1))
            val mid = Word(w.ls.take(j).drop(i + 1))
            // println(s"$w becomes $head, $mid, $tail split at ($i : ${Word(Vector(w.ls(i)))}, $j : ${Word(Vector(w.ls(j)))})")
            Task.parMap3(recNorm(head, threshold, maximize), recNorm(tail, threshold, maximize), recNorm(mid, threshold, maximize)){case (x, y, z) => x + y + z}
        }
    }

    def averageActual(length: Int, sampleSize: Int) : Task[Double] = {
        val ws = (1 to sampleSize).map(_ => ProofFinder.randomWord(length).reduce)
        val total = Task.gather(ws.map(w => normTask(w.ls))).map(_.sum)
        total.map(t => t / (sampleSize * length))
    }

    def averageHybrid(length: Int, threshold: Int, sampleSize: Int, maximize: Boolean = false) : Task[Double] = {
        val ws = (1 to sampleSize).map(_ => ProofFinder.randomWord(length).reduce)
        val total = Task.gather(ws.map(w => recNorm(w, threshold, maximize))).map(_.sum)
        total.map(t => t / (sampleSize * length))
    }

    def averageList(step: Int, sampleSize: Int = 100) = 
        LazyList.from(1).map(_ * step).map{n => n -> averageActual(n, sampleSize).runToFuture}
}