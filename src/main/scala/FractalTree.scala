import scala.annotation.tailrec
import scala.io.StdIn.readInt

object FractalTree extends App {

  //method to create stem of the tree
  @tailrec
  def buildStem(tree:Array[Array[Char]],point:Point,height:Int):Array[Array[Char]] = {
    if(height <= 0) tree
    else {
      tree(point.row)(point.col) = '|'
      buildStem(tree,Point(point.row-1,point.col),height-1)
    }
  }

  //generate left branch of the tree
  @tailrec
  def buildLeftBranch(tree: Array[Array[Char]],point:Point,height:Int): Array[Array[Char]] = {
    if (height <= 0) tree
    else {
      tree(point.row)(point.col) = '\\'
      buildLeftBranch(tree, Point(point.row - 1, point.col-1), height - 1)
    }
  }

  //generate right branch of the tree
  @tailrec
  def buildRightBranch(tree: Array[Array[Char]], point: Point, height: Int): Array[Array[Char]] = {
    if (height <= 0 ) tree
    else {
      tree(point.row)(point.col) = '/'
      buildRightBranch(tree, Point(point.row - 1, point.col + 1), height - 1)
    }
  }

  //combining left and right branch build
  def buildBranch(baseTree: Array[Array[Char]], point: Point, height: Int) = {
    buildRightBranch(buildLeftBranch(baseTree, Point(point.row - height, point.col - 1), height), Point(point.row - height, point.col + 1), height)
  }

  def build(baseTree:Array[Array[Char]],point:Point,height:Int,n:Int):Array[Array[Char]] = {
    if(n<1) baseTree
    else {
        //current tree build
        buildStem(baseTree, point, height)
        buildBranch(baseTree, point, height)

        //build left side trees
        build(baseTree,Point(point.row - (height*2), point.col - height), height/2, n-1)

        //build right side trees
        build(baseTree,Point(point.row - (height*2), point.col + height), height/2, n-1)
      }
  }


  //fractal tree execution starts
  val n = readInt()
  require(n>=1 && n<=5)

  build(Array.fill(63)(Array.fill(100)('_')), Point(62,49), 16, n)
    .foreach(row => {
      row.foreach(c => print(c))
      println()
    })

}