import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.PrivateMethodTester
import KmpSearch._

class KmpSearchSpec extends AnyFlatSpec with Matchers with PrivateMethodTester {
  val makeLpsTable: PrivateMethod[Array[Int]] = PrivateMethod[Array[Int]]('makeLpsTable)
  "makeLpsTable method" should "generate a pattern that contains Longest possible prefix" in {

    //private method invoker object

    KmpSearch invokePrivate  makeLpsTable("a") should equal(Array(-1))
    KmpSearch invokePrivate makeLpsTable("def") should equal(Array(-1,0,0))
    KmpSearch invokePrivate makeLpsTable("aaba") should equal(Array(-1,0,1,0))
    KmpSearch invokePrivate makeLpsTable("aaaa") should equal(Array(-1,0,1,2))
    KmpSearch invokePrivate makeLpsTable("computer") should equal(Array(-1,0,0,0,0,0,0,0))
    KmpSearch invokePrivate makeLpsTable("comcom") should equal(Array(-1,0,0,0,1,2))
  }

  it should "give illegal argument exception for empty string pattern" in {
    assertThrows[IllegalArgumentException](KmpSearch invokePrivate makeLpsTable(""))
  }

  "search method" should "return true, if pattern match in text" in {
    assert(search("mask","ma" ))
    assert(search("a","a"))
    assert(search("video","video"))
  }

  it should "return true, if empty string is given as pattern" in {
    assert(search("testing",""))
    assert(search("hello",""))
  }

  it should "return false, if pattern is not matching in text" in {
    assertResult(false)(search("matcher","ate"))
    search("testing","get") should be(false)
    search("testing"," ") should be(false)
  }
}
