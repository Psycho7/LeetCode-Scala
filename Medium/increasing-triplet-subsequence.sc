object Solution {
  def increasingTriplet(nums: Array[Int]): Boolean = {
    val f = Array.fill(4)(Int.MaxValue)
    f(0) = Int.MinValue
    var len = 1
    nums.foreach(x => {
      val t = (0 to len).find(x <= f(_)).get
      f(t) = x
      if (t == len) len += 1
      if (len > 3) return true
    })
    false
  }
}

Solution.increasingTriplet(Array(1, 2, 3, 4, 5))
Solution.increasingTriplet(Array(1, 1, 2, 2, 2))