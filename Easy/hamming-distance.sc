object Solution {
  def hammingDistance(x: Int, y: Int): Int = {
    def count(v: Int, s: Int): Int =
      if (v == 0) s else count(v >> 1, s + (v & 1))
    count(x ^ y, 0)
  }
}

Solution.hammingDistance(1, 4)