object Solution {
  def pacificAtlantic(matrix: Array[Array[Int]]): List[Array[Int]] = {
    type Pos = (Int, Int)
    if (matrix.isEmpty || matrix(0).isEmpty) return Nil
    val m = matrix.length
    val n = matrix(0).length
    val dir = Array((-1, 0), (+1, 0), (0, +1), (0, -1))

    def nextBatch(x: Int, y: Int) =
      dir.map{case (dx, dy) => (dx + x, dy + y)}
        .filter{case (a, b) => a >= 0 && a < m && b >=0 && b < n}

    def bfs(q: List[Pos], s: Set[Pos]): Set[Pos] = {
      if (q.isEmpty) s
      else {
        val (x, y) = q.head
        val ns = nextBatch(x, y)
          .filter(!s.contains(_))
          .filter{case (a, b) => matrix(x)(y) <= matrix(a)(b)}
        bfs(q.tail ++ ns, s + ((x, y)))
      }
    }

    val pq = (0 until m).map((_, 0)) ++ (0 until n).map((0, _))
    val aq = (0 until m).map((_, n - 1)) ++ (0 until n).map((m - 1, _))
    val ps = bfs(pq.toList, Set.empty)
    val as = bfs(aq.toList, Set.empty)
    (ps & as).toList.map{case (a, b) => Array(a, b)}
  }
}