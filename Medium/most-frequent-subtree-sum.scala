import scala.collection.mutable

/**
  * Definition for a binary tree node.
  * class TreeNode(var _value: Int) {
  *   var value: Int = _value
  *   var left: TreeNode = null
  *   var right: TreeNode = null
  * }
  */
object Solution {
  def findFrequentTreeSum(root: TreeNode): Array[Int] = {
    var m = mutable.Map[Int, Int]()
    def sum(n: TreeNode): Int = n match {
      case null => 0
      case _ =>
        val x = n.value + sum(n.left) + sum(n.right)
        m += (x -> (1 + m.getOrElse(x, 0)))
        x
    }

    if (root == null) return Array()
    sum(root)
    val v = m.maxBy(_._2)._2
    m.filter(_._2 == v).keys.toArray
  }
}