object Solution {
  def fractionToDecimal(numerator: Int, denominator: Int): String = {
    def decimal(num: Long, de: Long, ls: List[Int], xs: List[Int]): String =
    	if (num == 0) ls.reverse.mkString
    	else {
        val n = num / de;
        val m = num % de;
        val p = xs indexOf m
        if (p == -1) decimal(m * 10, de, n.toInt :: ls, m.toInt :: xs)
        else {
        	val (a, b) = n :: ls splitAt p + 1
          b.reverse.mkString + "(" + a.reverse.mkString + ")"
        }
      }
    val flag = numerator.toLong * denominator.toLong < 0
    val a: Long = Math.abs(numerator.toLong)
    val b: Long = Math.abs(denominator.toLong)
    val x = a / b
    val y = a % b
    val ans = if (y == 0) x.toString
      else x.toString + '.' + decimal(y * 10, b, Nil, List(y.toInt))
    if (flag) "-" + ans else ans
  }
}