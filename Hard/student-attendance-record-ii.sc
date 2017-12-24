object Solution {
  type Mat = Array[Array[Int]]

  val indices = 0 until 6

  def checkRecord(n: Int): Int = {
    val f = newMat
    f(0)(0) = 1
    val w = Array(
      Array(1, 1, 0, 1, 0, 0),
      Array(1, 0, 1, 1, 0, 0),
      Array(1, 0, 0, 1, 0, 0),
      Array(0, 0, 0, 1, 1, 0),
      Array(0, 0, 0, 1, 0, 1),
      Array(0, 0, 0, 1, 0, 0)
    )
    val r = mul(f, power(w, n + 1))
    r(0)(3)
  }

  def newMat = Array.fill(6, 6)(0)

  def mul(x: Mat, y: Mat): Mat = {
    val res = newMat
    for {
      i <- indices
      j <- indices
      k <- indices
    } {
      val m = x(i)(k).toLong * y(k)(j) % 1000000007
      res(i)(j) = (res(i)(j) + m.toInt) % 1000000007
    }
    res
  }

  def power(x: Mat, n: Int): Mat = n match {
    case 0 =>
      val r = newMat
      for (i <- indices) r(i)(i) = 1
      r
    case 1 => x
    case _ =>
      val h = power(x, n / 2)
      val r = power(x, n & 1)
      mul(mul(h, h), r)
  }
}

Solution.checkRecord(1)
Solution.checkRecord(2)
Solution.checkRecord(3)
Solution.checkRecord(4)
Solution.checkRecord(50)
Solution.checkRecord(100)