(set-logic AUFNIRA)
(declare-fun u() Real)
(declare-fun v() Real)
(declare-fun Ax() Real)
(declare-fun Ay() Real)
(declare-fun U() Real)
(declare-fun V() Real)
(declare-fun Px() Real)
(declare-fun Py() Real)
(declare-fun Qx() Real)
(declare-fun Qy() Real)
(declare-fun Rx() Real)
(declare-fun Ry() Real)
(assert (= (* (- Py 0.0) (- (* Ax (+ U 1.0)) 1.0)) (* (- (* Ay (+ U 1.0)) 0.0) (- Px 1.0))))
(assert (= (* (- Py Ay) (- (+ 1.0 u) Ax)) (* (- 0.0 Ay) (- Px Ax))))
(assert (= (* (- Qy 0.0) (- (* Ax (+ (+ U V) 1.0)) 1.0)) (* (- (* Ay (+ (+ U V) 1.0)) 0.0) (- Qx 1.0))))
(assert (= (* (- Qy Ay) (- (+ (+ 1.0 u) v) Ax)) (* (- 0.0 Ay) (- Qx Ax))))
(assert (= (* (- Ry 0.0) (- (* Ax (+ (+ U V) 1.0)) (+ 1.0 u))) (* (- (* Ay (+ (+ U V) 1.0)) 0.0) (- Rx (+ 1.0 u)))))
(assert (= (* (- Ry (* Ay (+ U 1.0))) (- (+ (+ 1.0 u) v) (* Ax (+ U 1.0)))) (* (- 0.0 (* Ay (+ U 1.0))) (- Rx (* Ax (+ U 1.0))))))
(assert (>= u 0.0))
(assert (>= v 0.0))
(assert (>= Ay 0.0))
(assert (>= U 0.0))
(assert (>= V 0.0))
(assert (not (= (* (- Qy Py) (- Rx Px)) (* (- Ry Py) (- Qx Px)))))
(assert (= u 0.7077071711828457))
(assert (= v 1.3286553548374274))
(assert (= U 0.7663723442057009))
(check-sat)
