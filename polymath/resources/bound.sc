// run using mill -i freegroups.repl
// cut and paste commands in sequence
import freegroups._, LinearNorm._
def vecn(n: Int) =
  Vector.fill(n)(Vector(1, 2, -1, -2)).foldRight(Vector(1))(_ ++ _)
def taskn(n: Int) = scaledTask(vecn(n), 1, 20)
import monix.execution.Scheduler.Implicits.global
def futn(n: Int) =
  taskn(n).runAsync.foreach((res) => println(s"norm: ${res.min} for n: $n"))
futn(1)
futn(2)
futn(3)
futn(6)
