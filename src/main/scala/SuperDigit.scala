import scala.annotation.tailrec

object SuperDigit {

  //method to find super digit
  @tailrec
  def superDigit(n: BigInt, acc: BigInt = 0): BigInt = {
    (n<10, acc+n <10) match {
      case (true, true) => acc + n
      case (true, false) => superDigit((n + acc) / 10, (n + acc) % 10)
      case (false, _) => superDigit(n / 10, acc + n % 10)
    }
  }

  //method to generate tens places to help repeater ==> 10, 100, 1000 ...
  @tailrec
  def generateAppender(n: Int, acc: BigInt = 1): BigInt = {
    if (n < 1) acc
    else generateAppender(n - 1, 10 * acc)
  }

  //generates a number with value repeated mentioned times => repeater(123,3) = 123123123
  def repeater(value: Int, repeat: Int): BigInt = {
    @tailrec
    def auxRepeater(n: Int, appender: BigInt, r: Int, acc: BigInt = 0): BigInt = {
      if (r < 1) acc
      else auxRepeater(n, appender, r - 1, (acc * appender) + n)
    }
    auxRepeater(value, generateAppender(value.toString.length), repeat)
  }
}
