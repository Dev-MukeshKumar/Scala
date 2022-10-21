import scala.io.StdIn.readInt
import scala.io.StdIn.readLine

object Main extends App{
  val n = readInt()
  val inputArray = Array.ofDim[String](n * 2)

  for (i <- 0 until n * 2)
    inputArray(i) = readLine()

  for (i <- 0 until n * 2 by 2) {
    if (KmpSearch.search(inputArray(i), inputArray(i + 1))) println("Yes")
    else println("No")
  }
}
