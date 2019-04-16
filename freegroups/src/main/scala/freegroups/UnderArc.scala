package freegroups

import scala.collection.mutable.{Map => MMap}

object UnderArc {
  val memopad: MMap[(Int, Int), Double] = MMap()

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

  def memoCount(n: Int, k: Int): Double =
    memopad.getOrElse(
      n -> k, {
        val result: Double =
          if (n == k) math.pow(2, n + 1) - 1
          else {
            val terms =
              for {
                a <- 0 to n - 2
                b <- 0 to n - 2 - a
                k1 <- 0 to k - a
              } yield
                (math.pow(3, a + 1) * memoCount(b, k1) * memoCount(
                  n - 2 - a - b,
                  k - a - k1
                ))
            terms.sum
          }
        memopad += (n, k) ->result 
        result
      }
    )

      def p(n: Int, k: Int) : Double = memoCount(n, k) / math.pow(4, n)
}
