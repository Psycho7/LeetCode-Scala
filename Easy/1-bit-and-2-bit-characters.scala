object Solution {
  def isOneBitCharacter(bits: Array[Int]): Boolean = {
    val len = bits.length

    def iter(pos: Int): Boolean =
      if (pos < len - 1) iter(if (bits(pos) == 0) pos + 1 else pos + 2)
      else pos == len - 1

    iter(0)
  }
}