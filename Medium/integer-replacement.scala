// TODO: Bit manipulation is more efficent
object Solution {
  def integerReplacement(n: Int): Int = {
    def next(x: Long) = if (x % 2 == 0) List(x / 2) else List(x - 1, x + 1)

    def iter(p: Int, s: Set[Long]): Int =
      if (s contains 1) p
      else iter(p + 1, s flatMap { next _ })

    iter(0, Set(n.toLong))
  }
}