package superficial

/**
  * A formal sum of elements of `A` with coefficients integers.
  */
case class FormalSum[A](coeffs: Map[A, Int]) {
  val coeffVec = coeffs.toVector

  def ++(that: FormalSum[A]) = FormalSum.reduced(coeffVec ++ that.coeffVec)

  def +(el: A) = FormalSum.reduced(coeffVec :+ (el -> 1))

  def -(el: A) = FormalSum.reduced(coeffVec :+ (el -> -1))

  def map[B](f: A => B): FormalSum[B] =
    FormalSum.reduced(coeffVec.map { case (a, n) => (f(a), n) })

  def flatMap[B](f: A => FormalSum[B]): FormalSum[B] = {
    val cv =
      for {
        (a, n) <- coeffVec
        (b, m) <- f(a).coeffVec
      } yield (b, n * m)
    FormalSum.reduced(cv)
  }
}

object FormalSum {

  /**
    * reduce a formal sum by combining terms and removing zero terms
    */
  def reduced[A](v: Vector[(A, Int)]) = {
    val m = v
      .groupBy(_._1)
      .view
      .mapValues { vc =>
        vc.map(_._2).sum
      }
      .filter(_._2 != 0)
      .toMap
    FormalSum(m)
  }

  /**
    * second boundary map for homology
    */
  def del2(c2: FormalSum[Polygon]) = c2.flatMap(_.del)

  /**
    * first boundary map for homology
    */
  def del1(c1: FormalSum[Edge]) = c1.flatMap(_.del)
}

