import smtwrap._ 

val n = SMTExpr("n")

val exp = (n * n) <= 1

exp.assert

0 <= n 