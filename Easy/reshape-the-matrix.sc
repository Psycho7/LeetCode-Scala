object Solution {
  def matrixReshape(nums: Array[Array[Int]], r: Int, c: Int): Array[Array[Int]] = {
    val count = nums.length * nums.head.length
    if (count != r * c) {
      nums
    } else {
      def func(xs: Array[Int], res: List[Array[Int]]): List[Array[Int]] = {
        val (head, tail) = xs.splitAt(c)
        if (tail.isEmpty) head :: res else func(tail, head :: res)
      }
      func(nums.flatten, Nil).reverse.toArray
    }
  }
}

Solution.matrixReshape(Array(Array(1, 2), Array(3, 4)), 1, 4).map(_.mkString(" "))
Solution.matrixReshape(Array(Array(1, 2), Array(3, 4)), 4, 1).map(_.mkString(" "))
Solution.matrixReshape(Array(Array(1, 2), Array(3, 4), Array(5, 6)), 4, 1).map(_.mkString(" "))
Solution.matrixReshape(Array(Array(1, 2), Array(3, 4), Array(5, 6)), 2, 3).map(_.mkString(" "))