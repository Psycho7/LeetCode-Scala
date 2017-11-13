object Solution {
  def evalRPN(tokens: Array[String]): Int = {
    def op(ls: List[Int], f: (Int, Int) => Int) =
      f(ls(1), ls.head) :: ls.drop(2)

    def iter(n: Int, ls: List[Int]): Int = {
      if (n == tokens.length) ls.head
      else tokens(n) match {
        case "+" => iter(n + 1, op(ls, _ + _))
        case "-" => iter(n + 1, op(ls, _ - _))
        case "*" => iter(n + 1, op(ls, _ * _))
        case "/" => iter(n + 1, op(ls, _ / _))
        case x => iter(n + 1, x.toInt :: ls)
      }
    }

    iter(0, Nil)
  }
}