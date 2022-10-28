import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import FractalTree._

class FractalTreeSpec extends AnyFlatSpec with Matchers{
  "FractalTree with n=1" should "have branches at point p1(31,33) & p2(31,65) and a row of '_' above p1 and p2" in {
    val fractalTree = buildTree(Array.fill(63)(Array.fill(100)('_')), Point(62,49), 16, 1)
    fractalTree(30) should equal(Array.fill(100)('_'))
    fractalTree(31)(33) should be('\\')
    fractalTree(31)(65) should be('/')
  }

  "FractalTree with n=2" should "have branches at point p1(15,25) p2(15,41) and p3(15,57) p4(15,73) and a row of '-' above p1 & p4" in {
    val fractalTree = buildTree(Array.fill(63)(Array.fill(100)('_')), Point(62, 49), 16, 2)
    fractalTree(14) should equal(Array.fill(100)('_'))
    //left tree
    fractalTree(15)(25) should be('\\')
    fractalTree(15)(41) should be('/')
    //right tree
    fractalTree(15)(57) should be('\\')
    fractalTree(15)(73) should be('/')
  }

  "FractalTree with n=3" should "have branches at row 7 and row 6 wil be '_'" in {
    val fractalTree = buildTree(Array.fill(63)(Array.fill(100)('_')), Point(62, 49), 16, 3)
    fractalTree(6) should equal(Array.fill(100)('_'))
    fractalTree(7) should not equal(Array.fill(100)('_'))
  }

  "FractalTree with n=4" should "have branches at row 3 and row 2 wil be '_'" in {
    val fractalTree = buildTree(Array.fill(63)(Array.fill(100)('_')), Point(62, 49), 16, 4)
    fractalTree(2) should equal(Array.fill(100)('_'))
    fractalTree(3) should not equal (Array.fill(100)('_'))
  }

  "FractalTree with n=5" should "have branches at row 1 and row 0 wil be '_'" in {
    val fractalTree = buildTree(Array.fill(63)(Array.fill(100)('_')), Point(62, 49), 16, 5)
    fractalTree(0) should equal(Array.fill(100)('_'))
    fractalTree(1) should not equal (Array.fill(100)('_'))
  }

  "FractalTree with n<1 and n>5" should "return 63x100 matrix filled with '_'" in {
    buildTree(Array.fill(63)(Array.fill(100)('_')), Point(62, 49), 16, -1) should equal (Array.fill(63)(Array.fill(100)('_')))
    buildTree(Array.fill(63)(Array.fill(100)('_')), Point(62, 49), 16, 0) should equal (Array.fill(63)(Array.fill(100)('_')))
    buildTree(Array.fill(63)(Array.fill(100)('_')), Point(62, 49), 16, 7) should equal (Array.fill(63)(Array.fill(100)('_')))
  }

}
