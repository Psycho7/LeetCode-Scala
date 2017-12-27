import scala.collection.immutable.TreeSet

object Solution {
  def reversePairs(nums: Array[Int]): Int = {
    if (nums.isEmpty) return 0

    val arr = TreeSet(nums: _*).toArray
    val map = arr.zipWithIndex.toMap

    def find(v: Int): Int = {
      def func(l: Int, r: Int): Int = {
//        println(s"Find $v   $l - $r")
        if (l >= r) l
        else {
          val mid = (l + r) >> 1
          if (v == arr(mid)) mid + 1
          else if (v < arr(mid)) func(l, mid) else func(mid + 1, r)
        }
      }
      func(0, arr.length)
    }

    val size = arr.length + 1
    val tree = new STree(size)
    nums.indices.foldLeft(0)((a, x) => {
      val v = nums(x)
      val idx = map(v)
      var db = v * 2
      if (db < 0 && v > 0) db = Int.MaxValue
      else if (db >0 && v < 0) db = Int.MinValue
      val up = find(db)
      val t = x - tree.query(0, up)
//      println(s"$x, ${nums(x)}, ${nums(x) * 2}, $idx, $up, ${tree.query(0, up)}, $t")
      tree.add(idx)
      a + t
    })
  }

  class STree(val size: Int) {
    val f = Array.fill(size << 2)(0)

    def add(index: Int): Unit = {
      def func(n: Int, start: Int, end: Int): Unit ={
//        println(s"$n, $start, $end -- $index, $size")
        f(n) += 1
        if (start + 1 < end) {
          val mid = (start + end) >> 1
          if (index < mid) func(n << 1, start, mid) else func(n << 1 | 1, mid, end)
        }
      }
      func(1, 0, size)
    }

    def query(left: Int, right: Int): Int = {
      def func(n: Int, start: Int, end: Int, l: Int, r: Int): Int = {
        // println(s"$n, $start, $end, $l, $r")
        if (start >= end || l >= r) 0
        else if (start == l && end == r) f(n)
        else {
          val mid = (start + end) >> 1
          val leftR = func(n << 1, start, mid, l, Math.min(mid, r))
          val rightR = func(n << 1 | 1, mid, end, Math.max(mid, l), r)
          leftR + rightR
        }
      }
      func(1, 0, size, left, Math.min(size, right))
    }
  }
}

Solution.reversePairs(Array(1,3,2,3,1))
Solution.reversePairs(Array(-5, -5))