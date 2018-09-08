// to run this, open REPL with:
// `mill -i freegroups.repl`
// In the REPL session
// ```
// import $exec.boundtask, boundtask._
// run()
// ```
import freegroups._, LinearNorm._
import monix.execution.Scheduler.Implicits.global
def vecn(n: Int) =
  Vector.fill(n)(Vector(1, 2, -1, -2)).foldRight(Vector(1))(_ ++ _)
def taskn(n: Int) = scaledTask(vecn(n), 1, 20)
val task =
  for {
    _ <- taskn(1)
    _ <- taskn(2)
    _ <- taskn(3)
    res <- taskn(6)
  } yield res
def run() =
  task.runAsync.foreach((res) => println(s"norm: ${res.min} for n: 6, giving bound ${(res.min.toDouble + 1) /  6} for the commutator"))
