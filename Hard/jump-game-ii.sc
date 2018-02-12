object Solution {
  def jump(nums: Array[Int]): Int = {
    val max = nums.indices.map{x => x + nums(x)}.scanLeft(0)(Math.max(_, _)).tail
    def iter(dep: Int, now: Int): Int = {
      if (now >= nums.length - 1) dep else iter(dep + 1, max(now))
    }
    iter(0, 0)
  }
}

Solution.jump(Array(2,3,1,1,4))
Solution.jump(Array(10,9,8,7,6,5,4,3,2,1))