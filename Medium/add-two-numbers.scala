object Solution {
  def addTwoNumbers(l1: ListNode, l2: ListNode): ListNode = {
    def iter(x: ListNode, y: ListNode, c: Int): ListNode = (x, y, c) match {
      case (null, null, 0) => null
      case (null, null, _) => new ListNode(c)
      case (null, _, _) => iter(y, x, c)
      case _ => {
        val v = x.x + c + (if (y == null) 0 else y.x)
        val node = new ListNode(v % 10)
        val next = iter(x.next, if (y == null) null else y.next, v / 10)
        node.next = next
        node
      }
    }

    iter(l1, l2, 0)
  }
}