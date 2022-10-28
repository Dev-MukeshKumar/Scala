import SuperDigit._
import scala.io.StdIn.readInt

object ExecuteSuperDigit extends App{

  //read inputs from user
  val n = readInt()
  val k = readInt()

  val validateInputAndGenerateMessage = (n<1 , k<1) match {
    case (false,false) => superDigit(repeater(n,k))
    case _ => "Enter non negative inputs"
  }

  println(validateInputAndGenerateMessage)
}
