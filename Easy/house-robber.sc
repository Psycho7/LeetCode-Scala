object Solution {
  def rob(nums: Array[Int]): Int = {
    val (x, y) = nums.foldLeft((0, 0)){
      case ((a, b), n) => (Math.max(a, b), Math.max(a + n, b))
    }
    Math.max(x, y)
  }
}

Solution.rob(Array(1, 2, 1, 5, 7, 10, 5))