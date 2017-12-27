import scala.collection.immutable.TreeSet

object Solution {
  def countSmaller(nums: Array[Int]): List[Int] = {
    val arr = TreeSet(nums: _*).toArray

//    def find(v: Int): Int = {
//      def func(l: Int, r: Int): Int = {
//        if (l >= r) -1
//        else {
//          val mid = (l + r) >> 1
//          val key = arr(mid)
//          if (key == v) mid
//          else if (v < key) func(l, mid) else func(mid + 1, r)
//        }
//      }
//
//      func(0, arr.length)
//    }

    val find = arr.zipWithIndex.toMap

    val tree = new STree(arr.length)
    nums.foldRight(List.empty[Int])((v, ns) => {
      val idx = find(v)
      tree.add(idx)
      tree.query(0, idx) :: ns
    })
  }

  class STree(val size: Int) {
    val f = Array.fill(size << 2)(0)

    def add(index: Int): Unit = {
      def func(n: Int, start: Int, end: Int): Unit = {
        f(n) += 1
        if (start + 1 < end) {
          val mid = (start + end) >> 1
          if (index < mid) func(n << 1, start, mid)
          else func(n << 1 | 1, mid, end)
        }
      }
      func(1, 0, size)
    }

    def query(left: Int, right: Int): Int = {
      def func(n: Int, start: Int, end: Int, l: Int, r: Int): Int = {
        if (start >= end || l >= r) 0
        else if (start == l && end == r) f(n)
        else {
          val mid = (start + end) >> 1
          val leftR = func(n << 1, start, mid, l, Math.min(mid, r))
          val rightR = func(n << 1 | 1, mid, end, Math.max(mid, l), r)
          leftR + rightR
        }
      }
      func(1, 0, size, left, right)
    }
  }
}

Solution.countSmaller(Array(5, 2, 6, 1, 1))