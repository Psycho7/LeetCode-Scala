object Solution {
  def generateMatrix(n: Int): Array[Array[Int]] = {
    val dx = Array(0, 1, 0, -1)
    val dy = Array(1, 0, -1, 0)
    val result = Array.ofDim[Int](n, n)
    def nextPos(x: Int, y: Int, way: Int) = (x + dx(way), y + dy(way))
    def func(x: Int, y: Int, v: Int, way: Int): Array[Array[Int]] = {
      if (v > n * n) result
      else {
        result(x)(y) = v
        val (nx, ny) = nextPos(x, y, way)
        if (nx >= n || nx < 0 || ny >= n || ny < 0 || result(nx)(ny) != 0) {
          val rw = (way + 1) & 3
          val (rx, ry) = nextPos(x, y, rw)
          func(rx, ry, v + 1, rw)
        } else {
          func(nx, ny, v + 1, way)
        }
      }
    }
    func(0, 0, 1, 0)
  }
}

Solution.generateMatrix(3).foreach(xs => println(xs.mkString(" ")))