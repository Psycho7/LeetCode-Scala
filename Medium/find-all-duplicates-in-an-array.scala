// TODO: Should AC but TLE

object Solution {
  def findDuplicates(nums: Array[Int]): List[Int] = {
    var ls = List.empty[Int]
    for (x <- 0 until nums.length) {
      val hash = nums(x).abs - 1
      if (nums(hash) < 0) ls = hash + 1 :: ls
      nums(hash) = -nums(hash)
    }
    ls
  }
}