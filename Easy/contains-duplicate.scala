object Solution {
  def containsDuplicate(nums: Array[Int]): Boolean = {
    nums.length != nums.toSet.size
  }
}