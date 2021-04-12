import smtwrap._ 

val n = IntSMTExpr("n")

val exp = (n * n) <= 4

exp.assert 

1 <= n 

val x = RealSMTExpr("x")
 
val exp2 = x + n

val exp3 = n.toReal + x

import scala.util._

n.declare

Try(exp.declare)

exp && !(exp2 <= 1.0) || (1.0 <= n)

val doc = SMTDoc(Vector(n, x), Vector(exp, 1 <= n))

val docGet = doc.valueGetter
print(docGet.docText)
val results = docGet.z3Run().out.chunks.toList.map(_.toString().trim)

val Right(m) = SMTDoc.parseValues(results) 

val m2Either = doc.seekValues()  
