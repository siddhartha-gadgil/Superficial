import freegroups._, LinearNormProofs._
import monix.execution.Scheduler.Implicits.global

def task(n: Int) =
  for {
    _ <- scaledTaskProofs(cna(1), 1, n, true)
    _ <- scaledTaskProofs(cna(2), 1, n, true)
    // _ <- fcna(3, n)
    // _ <- scaledTaskProofs(cna(6), 1, n, true)
    res <- scaledTaskProofs(c, 1, n, true)
  } yield res
  def run(k: Int) =
    task(k).runToFuture.foreach((res) => println(s"Proof:\n${res}"))