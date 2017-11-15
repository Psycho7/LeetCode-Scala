object Solution {
  def removeKdigits(num: String, k: Int): String = {
    def iter(p: Int, r: Int, ls: List[Char]): List[Char] = {
      if (p == num.length) ls.drop(r).reverse
      else if (r == 0) iter(p + 1, 0, num(p) :: ls)
      else ls match {
        case Nil => iter(p + 1, r,  List(num(p)))
        case x :: xs if num(p) < x => iter(p, r - 1, xs)
        case _ => iter(p + 1, r, num(p) :: ls)
      }
    }
    val ans = iter(0, k, Nil).dropWhile(_ == '0')
    if (ans.isEmpty) "0" else ans.mkString
  }
}