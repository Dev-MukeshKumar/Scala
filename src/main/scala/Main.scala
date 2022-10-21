object Main extends App{
  val n = scala.io.StdIn.readInt()
  val k = scala.io.StdIn.readInt()

  val x = SuperDigit.repeater(n,k)

  println(SuperDigit.superDigit(x))
}
