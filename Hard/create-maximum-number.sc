object Solution {
  def maxNumber(nums1: Array[Int], nums2: Array[Int], k: Int): Array[Int] = {
    def combine(n1: Array[Int], n2: Array[Int]): Array[Int] = {
      val ans = Array.fill(k)(0)
      var i, j, p = 0
      var flag = true
      while (p < k) {
        if (i < n1.length && j < n2.length) {
          val x = Array.concat(n1.drop(i), n2.drop(j))
          val y = Array.concat(n2.drop(j), n1.drop(i))
          flag = greater(x, y)
        } else flag = i < n1.length
        if (flag) {
          ans(p) = n1(i)
          i += 1
        } else {
          ans(p) = n2(j)
          j += 1
        }
        p += 1
      }
      ans
    }

    var ans = Array.empty[Int]
    for {
      x <- 0 to k
      y = k - x
      if x <= nums1.length && y <= nums2.length
    } {
      val t = combine(maxArray(nums1, x), maxArray(nums2, y))
      if (greater(t, ans)) ans = t
    }
    ans
  }

  def greater(n1: Array[Int], n2: Array[Int]): Boolean = {
    if (n1.length != n2.length) return n1.length > n2.length
    var i = 0
    while (i < n1.length && n1(i) == n2(i)) i += 1
    i < n1.length && n1(i) > n2(i)
  }

  def maxArray(ns: Array[Int], k: Int): Array[Int] = {
    val ans = Array.fill(k)(0)
    var p = 0
    for (i <- ns.indices) {
      while (ns.length - i + p > k && p > 0 && ans(p - 1) < ns(i)) p -= 1
      if (p < k) {
        ans(p) = ns(i)
        p += 1
      }
    }
    ans
  }
}

Solution.maxNumber(
  Array(6,7),
  Array(6,0,4),
  5
)

Solution.maxNumber(
  Array(3,4,6,5),
  Array(9,1,2,5,8,3),
  5
)