// to run this, open REPL with:
// `mill -i freegroups.repl`
// In the REPL session
// ```
// import $exec.boundtask, boundtask._
// run(10) 
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
    _ = println("1")
    _ <- taskn(2, k)
    _ = println("2")
    // _ <- taskn(3, k)
    _ <- taskn(6, k)
    _ = println("6")
    res <- commTask(k)
  } yield res
def run(k: Int) =
  task(k).runToFuture.foreach((res) => println(s"norm: ${res.min}"))

import monix.eval._

def preSeqN(n: Int, k: Int) = (1 to k).toVector.map(scaledNorm(vecn(n), _))

def commSeq(k: Int) = (1 to k).toVector.map(scaledNorm(Vector(1, 2, -1, -2), _))

def preTask(k: Int) = Task.sequence(preSeqN(1, k) ++ preSeqN(2, k) ++ preSeqN(6, k)) 

def fullTask(k: Int) = Task.sequence(preSeqN(1, k) ++ preSeqN(2, k) ++ preSeqN(6, k) ++ commSeq(k))

def fullRun(k: Int) = fullTask(k).runToFuture.foreach(_ => println(memoNorm(Vector(1, 2, -1, -2))))