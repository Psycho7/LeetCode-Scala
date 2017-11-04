//TODO: This one is more functional
//def countPrimes(n: Int): Int = {
//  def sieve(s: Stream[Int]): Stream[Int] =
//    s.head #:: sieve(s.tail filter (_ % s.head != 0))
//  val primes = sieve(Stream.from(2))
//  primes.takeWhile(_ <= n).length
//}
object Solution {
  def countPrimes(n: Int): Int = {
    val xs = Array.fill[Boolean](n)(true)

    def iter(p: Int): Int = {
      if (p > Math.sqrt(n)) xs.count(_ == true) - 2
      else {
        if (xs(p)) for (x <- p + 1 until n if x % p == 0) { xs(x) = false }
        iter(p + 1)
      }
    }
    if (n < 3) 0 else iter(2)
  }
}