object Solution {
  def summaryRanges(nums: Array[Int]): List[String] = {
    def iter(pos: Int, lo: Int, hi: Int, ls: List[String]): List[String] = {
      if (pos == nums.length) ((if (lo == hi) s"$lo" else s"$lo->$hi") :: ls).reverse
      else if (nums(pos) - hi == 1) iter(pos + 1, lo, hi + 1, ls)
      else iter(pos + 1, nums(pos), nums(pos),
        (if (lo == hi) s"$lo" else s"$lo->$hi") :: ls)
    }
    if (nums.isEmpty) Nil else iter(1, nums.head, nums.head, Nil)
  }
}