object Solution {
  def twoSum(nums: Array[Int], target: Int): Array[Int] = {
    def iter(pos: Int, map: Map[Int, Int]): Array[Int] = {
      val self = nums(pos)
      val other = target - self
      if (map contains other) Array(map(other), pos)
      else iter(pos + 1, map + (self -> pos))
    }

    iter(0, Map[Int, Int]())
  }
}