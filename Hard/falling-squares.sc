import Math.{max, min}

import scala.collection.immutable.TreeSet

object Solution {
  def fallingSquares(positions: Array[Array[Int]]): List[Int] = {
    val pos = TreeSet(positions.flatMap(a => Array(a(0), a(0) + a(1))): _*).toArray
    val find = pos.zipWithIndex.toMap

    val tree = new STree(pos.length)
    positions.map(arr => {
      val left = find(arr(0))
      val right = find(arr(0) + arr(1))
      val height = arr(1)
      val h = tree.query(left, right) + height
      tree.update(left, right, h)
      tree.query(0, pos.length)
    }).toList
  }

  class STree(size: Int) {
    val f = Array.fill(size << 2)(0)

    def update(left: Int, right: Int, value: Int): Unit = {
      def func(n: Int, start: Int, end: Int, l: Int, r: Int): Unit = {
        if (start >= end || l >= r) return
        f(n) = max(f(n), value)
        if (start + 1 < end) {
          val mid = (start + end) >> 1
          func(n << 1, start, mid, l, min(mid, r))
          func(n << 1 | 1, mid, end, max(mid, l), r)
        }
      }
      func(1, 0, size, left, right)
    }

    def query(left: Int, right: Int): Int = {
      def func(n: Int, start: Int, end: Int, l: Int, r: Int): Int = {
        if (start >= end || l >= r) return 0
        if (start == l && end == r) f(n) else {
          val mid = (start + end) >> 1
          val leftR = func(n << 1, start, mid, l, min(mid, r))
          val rightR = func(n << 1 | 1, mid, end, max(mid, l), r)
          max(leftR, rightR)
        }
      }
      func(1, 0, size, left, right)
    }
  }
}

Solution.fallingSquares(Array(Array(1, 2), Array(2, 3), Array(6, 1)))
Solution.fallingSquares(Array(Array(100, 100), Array(200, 100)))