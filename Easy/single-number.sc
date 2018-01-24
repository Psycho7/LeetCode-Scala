object Solution {
  def singleNumber(nums: Array[Int]): Int = (0 /: nums)(_ ^ _)
}

Solution.singleNumber(Array(1, 2, 1))
Solution.singleNumber(Array(1, 2, 2, 1, 7, 10, 7))