// TODO: This code is not functional at all

import scala.collection.mutable

object Solution {
  def trapRainWater(heightMap: Array[Array[Int]]): Int = {
    type Node = (Int, Int)
    val map = heightMap map { _.clone }
    val q = mutable.PriorityQueue.empty[Node](Ordering.by[Node, Int]({
      case (x, y) => map(x)(y)
    }).reverse)

    val n = heightMap.length
    if (n == 0) return 0
    val m = heightMap(0).length
    if (m == 0) return 0
    val visit = Array.fill(n)(Array.fill(m)(false))

    def func(x: Int, y: Int) = {
      visit(x)(y) = true
      q.enqueue((x, y))
    }

    for (x <- 0 until n) {
      func(x, 0)
      func(x, m - 1)
    }
    for (y <- 0 until m) {
      func(0, y)
      func(n - 1, y)
    }

    val dir = Array[Node]((1, 0), (-1, 0), (0, 1), (0, -1))

    while (q.nonEmpty) {
      val (x, y) = q.dequeue()
      for {
        (dx, dy) <- dir
        (nx, ny) = (x + dx, y + dy)
        if nx >= 0 && nx < n && ny >= 0 && ny < m
        if !visit(nx)(ny)
      } {
        visit(nx)(ny) = true
        if (map(nx)(ny) < map(x)(y)) map(nx)(ny) = map(x)(y)
        q.enqueue((nx, ny))
      }
    }

    map.map(_.sum).sum - heightMap.map(_.sum).sum
  }
}