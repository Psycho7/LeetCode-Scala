/**
  * Definition for a binary tree node.
  * class TreeNode(var _value: Int) {
  *   var value: Int = _value
  *   var left: TreeNode = null
  *   var right: TreeNode = null
  * }
  */

class TreeNode(var _value: Int) {
  var value: Int = _value
  var left: TreeNode = null
  var right: TreeNode = null
}

object Solution {
  def sumOfLeftLeaves(root: TreeNode): Int = func(root, false)

  def func(root: TreeNode, flag: Boolean): Int = root match {
    case null => 0
    case r if flag && r.left == null && r.right == null => r.value
    case r => func(r.left, true) + func(r.right, false)
  }
}