/**
  * Definition for singly-linked list.
  * class ListNode(var _x: Int = 0) {
  *   var next: ListNode = null
  *   var x: Int = _x
  * }
  */
case class ListNode(var x: Int = 0,
                    var next: ListNode = null) {
}


object Solution {
  def partition(head: ListNode, x: Int): ListNode = {
    val left, right = ListNode()
    var l = left
    var r = right
    var p = head
    while (p != null) {
      if (p.x < x) {
        l.next = ListNode(p.x)
        l = l.next
      } else {
        r.next = ListNode(p.x)
        r = r.next
      }
      p = p.next
    }
    l.next = right.next
    left.next
  }
}

val x = ListNode(1, ListNode(4, ListNode(3, ListNode(2))))
Solution.partition(x, 3)

