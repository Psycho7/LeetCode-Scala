object Solution {
  def isInterleave(s1: String, s2: String, s3: String): Boolean = {
    val f = Array.ofDim[Boolean](s3.length + 1, s1.length + 1, s2.length + 1)
    f(0)(0)(0) = true
    for {
      len <- 0 until s3.length
      i <- 0 to len
      if i <= s1.length
      j = len - i
      if j <= s2.length
      if f(len)(i)(j)
    } {
      val x = s3(len)
      if (i < s1.length && x == s1(i)) f(len + 1)(i + 1)(j) = true
      if (j < s2.length && x == s2(j)) f(len + 1)(i)(j + 1) = true
    }
    f(s3.length)(s1.length)(s2.length)
  }
}