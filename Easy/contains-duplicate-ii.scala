import scala.collection.mutable

object Solution {
  def containsNearbyDuplicate(nums: Array[Int], k: Int): Boolean = {
    var m = mutable.Map[Int, Int]()
    for (i <- nums.indices) {
      if (m.contains(nums(i)) && i - m(nums(i)) <= k) return true
      m(nums(i)) = i
    }
    false
  }
}