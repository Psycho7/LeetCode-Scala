object Solution {
  def checkValidString(s: String): Boolean = {
    val xs = s.scanLeft((0, 0)){
      case ((low, high), '(') => (low + 1, high + 1)
      case ((low, high), ')') => (Math.max(0, low - 1), high - 1)
      case ((low, high), '*') => (Math.max(0, low - 1), high + 1)
    }
//    println(xs.toList)
    !xs.exists(_._2 < 0) && xs.last._1 == 0
  }
}

//Solution.checkValidString("()")
//Solution.checkValidString("")
//Solution.checkValidString("(")
Solution.checkValidString("(*)")
//Solution.checkValidString("*")