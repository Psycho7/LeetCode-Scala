object Solution {
  def maxSubArray(nums: Array[Int]): Int = {
    nums.tail.scanLeft(nums.head){ Math.max(0, _) + _ }.max
  }
}

Solution.maxSubArray(Array(-2, -10, -15))