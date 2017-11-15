// TLE due to the low performance of Scala and wrong measurement of LeetCode
object Solution {
  def productExceptSelf(nums: Array[Int]): Array[Int] = {
    val n =nums.length
    val res = Array.fill(n)(0)
    res(0) = 1
    for (i <- 1 until n) res(i) = res(i - 1) * nums(i - 1)
    var r = 1
    for (i <- n - 1 to 0 by -1) {
      res(i) *= r
      r *= nums(i)
    }
    res
  }
}