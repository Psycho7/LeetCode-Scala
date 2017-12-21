object Solution {
  def maxSumOfThreeSubarrays(nums: Array[Int], k: Int): Array[Int] = {
    val presum = nums.scanLeft(0)(_ + _)
    val n = nums.length - k
    val sub = (0 to n).map(x => presum(x + k) - presum(x))
    val left = (1 to n).scanLeft(0) { (i, j) => if (sub(i) >= sub(j)) i else j }
    val right = (0 until n).scanRight(n) { (i, j) => if (sub(i) >= sub(j)) i else j }
    val mid = (k to n-k).maxBy(x => sub(left(x - k)) + sub(x) + sub(right(x + k)))
    Array(left(mid - k), mid, right(mid + k))
  }
}

Solution.maxSumOfThreeSubarrays(Array(1,2,1,2,6,7,5,1), 2)
Solution.maxSumOfThreeSubarrays(Array(1,2,1,2,6,7,5,7), 2)