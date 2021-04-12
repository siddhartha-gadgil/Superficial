import smtwrap._ 

val n = IntSMTExpr("n")

val exp = (n * n) <= 1

exp.assert 

0 <= n 

val x = RealSMTExpr("x")
 
val exp2 = x + n

val exp3 = n.toReal + x

import scala.util._

n.declare

Try(exp.declare)

exp && !(exp2 <= 1.0) || (1.0 <= n)