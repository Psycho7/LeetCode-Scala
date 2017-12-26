class NumArray(_nums: Array[Int]) {
  val size = _nums.length

  val f = Array.ofDim[Int](size << 2)

  (0 until size).foreach(i => update(i, _nums(i)))

  def update(i: Int, v: Int) {
    def func(n: Int, start: Int, end: Int): Int = {
      if (start + 1 == end) {
        val old = f(n)
        f(n) = v
        v - old
      } else {
        val mid = (start + end) >> 1
        val mod = if (i < mid) func(n << 1, start, mid) else func(n << 1 | 1, mid, end)
        f(n) = f(n) + mod
        mod
      }
    }
    func(1, 0, size)
  }

  def sumRange(i: Int, j: Int): Int = {
    def func(n: Int, start: Int, end: Int, l: Int, r: Int): Int = {
      if (start == l && end == r) f(n)
      else if (start >= end || l >= r) 0
      else {
        val mid = (start + end) >> 1
        val leftR = func(n << 1, start, mid, l, Math.min(mid, r))
        val rightR = func(n << 1 | 1, mid, end, Math.max(mid, l), r)
        leftR + rightR
      }
    }
    func(1, 0, size, i, j + 1)
  }
}

var obj = new NumArray(Array(9, -8))
obj.sumRange(1, 1)