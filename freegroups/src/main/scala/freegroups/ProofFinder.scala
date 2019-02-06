package freegroups

import monix.eval._, monix.tail._, cats.implicits._

import scala.collection.mutable.{Map => mMap}

import monix.execution.Scheduler.Implicits.global

import LinearNorm._ , LinearNormProofs.{c, cna}

object ProofFinder{
    def memScaledNorm(word: Word, n: Int) =
        for {
            t1 <- Task{println(s"word: $word, exponent: $n"); memoNorm.get(word.ls)}
            t2 <- scaledNorm(word.ls, n)
        } yield (word, n, t1, t2)

    def homogeneityTask(seq : Vector[(Word, Int)]) = 
        Task.sequence(
            seq.map{
                case (w, n) => memScaledNorm(w, n)
            }
        )

    def makeSeq(n: Int, target: Word, preCalcs : Word*) : Vector[(Word, Int)] = 
            preCalcs.toVector.flatMap(w =>
                (1 to n).toVector.map(j => w -> j) ) ++
                (1 to n).toVector.map(j => target -> j)

    def egSeq(n: Int) = makeSeq(n, c, cna(1), cna(2), cna(6))
    
    def egTask(n: Int) = homogeneityTask(egSeq(n))
}