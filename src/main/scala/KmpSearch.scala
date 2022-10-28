import scala.annotation.tailrec

object KmpSearch {

  private def makeLpsTable(pattern: String): Array[Int] = {
    val patternLength = pattern.length
    require(patternLength > 0)

    @tailrec
    def traversePattern(i: Int, j: Int, table: Array[Int] = Array(-1, 0)): Array[Int] = {
      if (j == patternLength - 1) table
      else {
        val isMatch = pattern(i) == pattern(j)
        val index = if (isMatch) table.last + 1 else 0
        traversePattern(if (isMatch) i + 1 else i, j + 1, table :+ index)
      }
    }

    if (patternLength == 1) Array(-1)
    else traversePattern(0, 1)
  }

  def search(text: String, pattern: String): Boolean = {
    val patternLength = pattern.length
    if (patternLength <= 0) true
    else {
      val textLength = text.length
      if (textLength < patternLength) false
      else {
        val table = makeLpsTable(pattern)

        @tailrec
        def checkPattern(i: Int, j: Int): Boolean = {
          if (textLength - i < patternLength - j)
            false
          else {
            (i < textLength, j < patternLength) match {
              case (true, true) =>
                if (text(i) == pattern(j)) {
                  checkPattern(i + 1, j + 1)
                } else {
                  val index = table(j)
                  val (_i, _j) = if (index == -1) (i + 1, 0) else (i, index)
                  checkPattern(_i, _j)
                }
              case (false, true) => false
              case _ => true
            }
          }
        }

        checkPattern(0, 0)
      }
    }
  }
}
