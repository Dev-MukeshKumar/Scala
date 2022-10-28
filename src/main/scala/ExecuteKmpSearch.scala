import scala.annotation.tailrec
import scala.io.StdIn.readInt
import scala.io.StdIn.readLine
import KmpSearch._

object ExecuteKmpSearch extends App{

  val n = readInt()
  traverseAndSearchPatterns(n,getArrayOfInputs(n))

  //method to get array of string and pattern inputs with pattern length validation
  @tailrec
  def getArrayOfInputs(n:Int,array: Array[String] = Array.empty[String],i: Int=0): Array[String] = {
    if(i >= n*2) array
    else {
      val input = readLine()
      (i==0,i%2!=0) match {
        case (true,_) => getArrayOfInputs(n,array :+ input,i+1)
        case (false,true) if array(i-1).length < input.length => {
          println(s"the pattern: '$input' for '${array(i - 1)}' exceeds its length! try other pattern.")
          getArrayOfInputs(n, array, i)
        }
        case _ => getArrayOfInputs(n,array :+ input,i+1)
      }
    }
  }

  @tailrec
  def traverseAndSearchPatterns(n:Int,array: Array[String],i:Int=0): Unit = {
    if (i < n*2){
      search(array(i), array(i + 1)) match {
        case true => println("Yes")
        case false => println("No")
      }
      traverseAndSearchPatterns(n, array, i+2)
    }
  }
}
