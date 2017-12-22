object Solution {
  def subsetsWithDup(nums: Array[Int]): List[List[Int]] = {
    val dis = nums.distinct
    val ns = Map(dis.map(n => (n, nums.count(_ == n))): _*)
    val raw = dis.map(x => (0 to ns(x)).map(List.fill(_)(x)))
    raw.foldLeft[List[List[Int]]](List(Nil)) { (x, y) =>
      for {i <- x; j <- y} yield i ::: j
    }
  }
}

Solution.subsetsWithDup(Array(1, 2, 2))