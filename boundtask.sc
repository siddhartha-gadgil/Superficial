// to run this, open REPL with:
// `mill -i freegroups.repl`
// In the REPL session
// ```
// import $exec.boundtask, boundtask._
// run(20)
// ```
import freegroups._, LinearNorm._
import monix.execution.Scheduler.Implicits.global
def vecn(n: Int) =
  Vector.fill(n)(Vector(1, 2, -1, -2)).foldRight(Vector(1))(_ ++ _)
def taskn(n: Int, k: Int) = scaledTask(vecn(n), 1, k)
def commTask(k: Int) = scaledTask(Vector(1, 2, -1, -2), 1, k)
def task(k: Int) =
  for {
    _ <- taskn(1, k)
    // _ = {println("1")}
    _ <- taskn(2, k)
    // _ = {println("2")}
    _ <- taskn(6, k)
    // _ = {println("6")}
    res <- commTask(k)
  } yield res
def run(k: Int) =
  task(k).runToFuture.foreach((res) => println(s"norm: ${res.min}"))
