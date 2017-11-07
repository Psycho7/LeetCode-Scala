object Solution {
  def longestWord(words: Array[String]): String = {
    if (words.length == 0) return ""
    var s = Set.empty[String]
    s = s + ""
    var ans = ""
    val arr = words.sorted
    for (str <- arr; if str.length > 0) {
      val len = str.length
      if (s contains str.substring(0, len - 1)) {
        s = s + str
        if (str.length > ans.length) ans = str
      }
    }
    ans
  }
}