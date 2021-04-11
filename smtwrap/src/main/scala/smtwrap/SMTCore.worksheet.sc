import smtwrap._ 

val n = IntSMTExpr("n")

val exp = (n * n) <= 1

exp.assert 

0 <= n 