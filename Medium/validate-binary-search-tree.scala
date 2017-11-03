/**
  * Definition for a binary tree node.
  * class TreeNode(var _value: Int) {
  * var value: Int = _value
  * var left: TreeNode = null
  * var right: TreeNode = null
  * }
  */
object Solution {
  def isValidBST(root: TreeNode): Boolean = {
    def valid(n: TreeNode, min: Long, max: Long): Boolean =
      if (n == null) true
      else if (n.value >= max || n.value <= min) false
      else valid(n.left, min, n.value) && valid(n.right, n.value, max)

    valid(root, Long.MinValue, Long.MaxValue)
  }
}