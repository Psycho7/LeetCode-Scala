object Solution {
  def candy(ratings: Array[Int]): Int = {
    val res = Array.fill(ratings.length)(1)
    for (i <- 0 until ratings.length - 1; if ratings(i) < ratings(i + 1))
      res(i + 1) = res(i) + 1
    for (i <- ratings.length - 1 to 1 by -1; if ratings(i - 1) > ratings(i))
      res(i - 1) = Math.max(res(i - 1), res(i) + 1)
    res.sum
  }
}

Solution.candy(Array(1, 3, 2, 2, 2, 3, 1, 3, 2))