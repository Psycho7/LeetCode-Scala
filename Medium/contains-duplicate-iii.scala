import scala.collection.mutable

object Solution {
  def containsNearbyAlmostDuplicate(nums: Array[Int], k: Int, t: Int): Boolean = {
    if (k == 0) return false
    var m = mutable.TreeSet[Long]()
    for (i <- nums.indices) {
      val n = nums(i)
      val less = m.to(n)
      val more = m.from(n)
      if (less.nonEmpty && n - less.max <= t || more.nonEmpty && more.min - n <= t)
        return true
      m += n
      if (i >= k ) m -= nums(i - k)
    }
    false
  }
}