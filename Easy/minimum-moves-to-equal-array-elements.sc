object Solution {
  def minMoves(nums: Array[Int]): Int = {
    nums.sum - nums.min * nums.length
  }
}

Solution.minMoves(Array(1, 2, 3))