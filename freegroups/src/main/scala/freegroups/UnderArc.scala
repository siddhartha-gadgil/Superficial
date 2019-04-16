package freegroups

import scala.collection.mutable.{Map => MMap}

object UnderArc {
  val memopad: MMap[(Int, Int, Int), Double] = MMap()

  def withUnmatched(n: Int, k: Int): Double =
    if (n == k) math.pow(2, n + 1) - 1
    else {
      val terms =
        for {
          a <- 0 to n - 2
          b <- 0 to n - 2 - a
          k1 <- 0 to k - a
        } yield
          (math.pow(3, a + 1) * withUnmatched(b, k1) * withUnmatched(
            n - 2 - a - b,
            k - a - k1
          ))
      terms.sum
    }

  def count(n: Int, k: Int, degree: Int): Double =
    memopad.getOrElse(
      (n, k, degree), {
        val result: Double =
          if (n == k) math.pow(2, n + 1) - 1
          else {
            val terms =
              for {
                a <- 0 to n - 2
                b <- 0 to n - 2 - a
                k1 <- 0 to k - a
                term1 = count(b, k1, 2) * count(
                  n - 2 - a - b,
                  k - a - k1, degree
                )
                term2 = (count(b, k1, 3) -count(b, k1, 2)) * count(
                  n - 2 - a - b,
                  k - a - k1, 1
                )
              } yield
                (math.pow(degree, a) * 3 * (term1 + term2)) 
            terms.sum
          }
        memopad += (n, k, degree) ->result 
        result
      }
    )

      def p(n: Int, k: Int, degree: Int = 3) : Double = count(n, k, degree) / math.pow(4, n)
}
