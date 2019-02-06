import freegroups._, LinearNormProofs._
import monix.execution.Scheduler.Implicits.global

import monix.eval._

def taskn(n: Int, k: Int) = Task.gatherUnordered((1 to k).map(j => computeScaledNormProof(cna(n), j, true)))
def commTask(k: Int) = scaledTaskProofs(c, 1, k, true)
def task(k: Int) =
  for {
    _ <- taskn(1, k)
    _ = println("1")
    _ <- taskn(2, k)
    _ = println("2")
    // _ <- taskn(3, k)
    _ <- taskn(6, k)
    _ = println("6")
    res <- commTask(k)
  } yield res
  def run(k: Int) =
    task(k).runToFuture.foreach((res) => println(s"Proof:\n${res}"))