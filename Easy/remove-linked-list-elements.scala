/**
  * Definition for singly-linked list.
  * class ListNode(var _x: Int = 0) {
  *   var next: ListNode = null
  *   var x: Int = _x
  * }
  *
  * TODO: This solution is not functional
  */
object Solution {
  def removeElements(head: ListNode, v: Int): ListNode = {
    val p = new ListNode()
    p.next = head
    var pre = p
    while (pre != null) {
      var next = pre.next
      while (next != null && next.x == v) next = next.next
      pre.next = next
      pre = next
    }
    p.next
  }
}