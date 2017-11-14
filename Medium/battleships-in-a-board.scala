object Solution {
  def countBattleships(board: Array[Array[Char]]): Int = {
    var ans = 0
    for {
      i <- board.indices
      j <- board.head.indices
      if board(i)(j) == 'X'
    } {
      ans += 1
      if ((i > 0) && (board(i - 1)(j) == 'X') ||
        (j > 0) && (board(i)(j - 1) == 'X')) ans -= 1
    }
    ans
  }
}