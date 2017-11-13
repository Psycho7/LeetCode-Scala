object Solution {
  def rotate(nums: Array[Int], k: Int): Unit = {
    def reverse(l: Int, r: Int) = {
      for (i <- l until (l + r) >> 1) {
        val j = l + r - 1 - i
        if (i != j) {
          nums(i) = nums(i) ^ nums(j)
          nums(j) = nums(i) ^ nums(j)
          nums(i) = nums(i) ^ nums(j)
        }
      }
    }
    val n = nums.length
    val m = k % n
    reverse(0, n)
    reverse(0, m)
    reverse(m, n)
  }
}