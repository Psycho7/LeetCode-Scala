// Two ways to DP
object SolutionA {
  def numDistinct(s: String, t: String): Int = {
    if (t.isEmpty) return 1
    if (s.isEmpty) return 0
    val memo = Array.fill(t.length)(Array.fill(s.length)(0))
    memo(0)(0) = if (t.head == s.head) 1 else 0
    for (j <- s.indices.tail)
      memo(0)(j) = memo(0)(j - 1) +
        (if (t.head == s(j)) 1 else 0)
    for {
      i <- t.indices.tail
      j <- s.indices.tail
    } memo(i)(j) = memo(i)(j - 1) +
      (if (t(i) == s(j)) memo(i - 1)(j - 1) else 0)
    memo.last.last
  }
}

object SolutionB {
  def numDistinct(s: String, t: String): Int = {
    if (t.isEmpty) return 1
    val memo = Array.fill(s.length)(Array.fill(t.length)(0))
    for {
      i <- s.indices
      if s(i) == t.head
    } memo(i)(0) = 1
    for {
      i <- s.indices
      j <- t.indices.tail
      if s(i) == t(j)
      k <- 0 until i
    } memo(i)(j) = memo(i)(j) + memo(k)(j - 1)
    var ans = 0
    for (i <- s.indices) ans += memo(i)(t.length - 1)
    ans
  }
}