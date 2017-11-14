object Solution {
  def numTrees(n: Int): Int = {
    val memo = Array.fill(n + 1)(0)
    memo(0) = 1
    memo(1) = 1
    for {
      i <- 2 to n
      j <- 1 to i
    } memo(i) += memo(j - 1) * memo(i - j)
    memo(n)
  }
}