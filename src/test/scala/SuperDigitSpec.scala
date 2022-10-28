import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SuperDigitSpec extends AnyFlatSpec with Matchers{
  //generateAppender test cases
  "generateAppender method" should "generate 10^x when valid integer given." in {
    SuperDigit.generateAppender(1) should be(10)
    assert(SuperDigit.generateAppender(1) == 10)
    assert(SuperDigit.generateAppender(2) == 100)
    assert(SuperDigit.generateAppender(5) == 100000)
  }

  it should "return 1, if zero or negative integer value given. " in {
    SuperDigit.generateAppender(0) shouldBe 1
    SuperDigit.generateAppender(-1) shouldBe 1
    SuperDigit.generateAppender(-6) shouldBe 1
  }

  //repeater method test cases
  "repeater method" should "repeat n by k times" in {
    SuperDigit.repeater(123,1) shouldBe 123
    SuperDigit.repeater(1,3) shouldBe 111
    SuperDigit.repeater(12,2) shouldBe 1212
  }

  it should "return 0 when k is zero or negative." in {
    SuperDigit.repeater(123,0) shouldBe 0
    SuperDigit.repeater(12,-4) shouldBe 0
    SuperDigit.repeater(198,-8) shouldBe 0
  }

  //superDigit method test cases
  "superDigit method" should "return sum of the digits until it becomes a single digit" in {
    SuperDigit.superDigit(12) shouldBe 3
    SuperDigit.superDigit(451) shouldBe 1
    SuperDigit.superDigit(34561) shouldBe 1
  }

  it should "return same input when the given input is less than 10" in {
    SuperDigit.superDigit(9) shouldBe 9
    SuperDigit.superDigit(-1) shouldBe -1
    SuperDigit.superDigit(-34) shouldBe -34
  }
}