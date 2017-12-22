object Solution {
  def maxProfit(k: Int, prices: Array[Int]): Int = {
    import Math._
    val op = min(k << 1, prices.length)
    val f = Array.ofDim[Int](2, op + 1, 2)
    f.foreach(_.foreach(x => {x(0) = -1 << 30; x(1) = -1 << 30}))
    f(0)(0)(1) = 0
    var ans = 0
    for (i <- 1 to prices.length) {
      val now = i & 1
      val pre = now ^ 1
      for (t <- 0 to min(op, i)) {
        f(now)(t)(0) = f(pre)(t)(0)
        f(now)(t)(1) = f(pre)(t)(1)
      }
      for (t <- 0 until min(op, i)) {
        f(now)(t + 1)(1) = max(f(now)(t + 1)(1), f(pre)(t)(0) + prices(i - 1))
        f(now)(t + 1)(0) = max(f(now)(t + 1)(0), f(pre)(t)(1) - prices(i - 1))
        ans = max(ans, f(now)(t + 1)(1))
      }
    }
    ans
  }
}
Solution.maxProfit(2, Array())
Solution.maxProfit(1, Array(1, 2))
Solution.maxProfit(2, Array(3, 5))
Solution.maxProfit(2, Array(5, 3))