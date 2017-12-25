object Solution {
  def getSkyline(buildings: Array[Array[Int]]): List[Array[Int]] = {
    val ts = collection.mutable.TreeSet.empty[Int]
    buildings.foreach(x => {
      ts.add(x(0))
      ts.add(x(1))
    })
    val m, rm = collection.mutable.HashMap.empty[Int, Int]
    var count = 0
    ts.foreach(x => {
      m.update(x, count)
      rm.update(count, x)
      count += 1
    })

    val tree = new STree(count)
    buildings.foreach(x => tree.update(m(x(0)), m(x(1)), x(2)))
    val tmp = (0 until count).map(idx => Array(rm(idx), tree.query(idx)))
    tmp.foldLeft(List(Array(0, 0)))((l, r) => if (l.head(1) != r(1)) r :: l else l).reverse.tail
  }

  class STree(val size: Int) {
    import Math.{min, max}

    val f = Array.ofDim[Int](size << 2)

    def update(l: Int, r: Int, v: Int): Unit = {
      def func(n: Int, start: Int, end: Int, left: Int, right: Int): Unit = {
        // println(s"$n, $start, $end, $left, $right --- ${query(1)}-${query(2)}")
        val mid = (start + end) >> 1
        if (start == left && end == right) f(n) = max(f(n), v)
        if (start + 1 != end) {
          if (start + 1 <= mid) func(n << 1, start, mid, left, min(right, mid))
          if (mid + 1 <= right) func(n << 1 | 1, mid, end, max(mid, left), right)
        }
      }
      func(1, 0, size, l, r)
    }

    def query(index: Int): Int = {
      def func(n: Int, start: Int, end: Int): Int = {
        if (start + 1 == end) f(n)
        else {
          val mid = (start + end) >> 1
          if (index < mid) func(n << 1, start, mid) else func(n << 1 | 1, mid, end)
        }
      }
      func(1, 0, size)
    }
  }
}

val x = Array(Array(2, 9, 10),
  Array(3, 7, 15),
  Array(5, 12, 12),
  Array(15,20,10),
  Array(19,24,8))
Solution.getSkyline(x).map(_.toList)