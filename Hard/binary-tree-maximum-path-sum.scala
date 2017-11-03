/**
  * Definition for a binary tree node.
  * class TreeNode(var _value: Int) {
  *   var value: Int = _value
  *   var left: TreeNode = null
  *   var right: TreeNode = null
  * }
  *
  * TODO: This solution is not pure functional
  */
object Solution {
  def maxPathSum(root: TreeNode): Int = {
    var ans = Int.MinValue
    def iter(n: TreeNode): Int = if (n == null) 0 else {
      val (l, r) = (iter(n.left), iter(n.right))
      ans = Math.max(ans, l + r + n.value)
      Math.max(0, Math.max(l, r) + n.value)
    }

    iter(root)
    ans
  }
}