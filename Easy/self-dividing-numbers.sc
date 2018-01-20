//object Solution {
//  def selfDividingNumbers(left: Int, right: Int): List[Int] = {
//    def check(n: Int, res: Int): Boolean = {
//      if (res == 0) return true
//      val rem = res % 10
//      if (rem == 0 || n % rem > 0) false else check(n, res / 10)
//    }
//    (left to right).filter(x => check(x, x)).toList
//  }
//}

object Solution {
  def selfDividingNumbers(left: Int, right: Int): List[Int] = {
    def check(n: Int) = {
      n.toString.forall(p => p != '0' && n % p.asDigit == 0)
    }
    (left to right).filter(check).toList
  }
}

Solution.selfDividingNumbers(1, 22)