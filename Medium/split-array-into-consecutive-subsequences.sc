object Solution {
  def isPossible(nums: Array[Int]): Boolean = {
    val dis = nums.distinct
    val count, tail = collection.mutable.Map(dis.map((_, 0)): _*)
    dis.foreach(k => count(k) = nums.count(_ == k))
    nums.foreach(x => {
      if (count.getOrElse(x, 0) != 0) {
        if (tail.getOrElse(x - 1, 0) > 0) {
          tail(x - 1) = tail(x - 1) - 1
          tail(x) = tail(x) + 1
        } else if (count.getOrElse(x + 1, 0) > 0 && count.getOrElse(x + 2, 0) > 0) {
          tail(x + 2) = tail(x + 2) + 1
          count(x + 1) = count(x + 1) - 1
          count(x + 2) = count(x + 2) - 1
        } else return false
        count(x) = count(x) - 1
      }
    })
    true
  }
}

Solution.isPossible(Array(1,2,3,4,4,5))
Solution.isPossible(Array(1,2,3,3,4,4,5,5))
Solution.isPossible(Array(3,4,4,5,6,7,8,9,10,11))