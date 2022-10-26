import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.PrivateMethodTester

class KmpSearchTest extends AnyFlatSpec with Matchers with PrivateMethodTester {
  val makeTable: PrivateMethod[Array[Int]] = PrivateMethod[Array[Int]]('makeTable)
  "makeTable method" should "generate a pattern that contains Longest possible prefix" in {

    //private method invoker object

    KmpSearch invokePrivate  makeTable("a") should equal(Array(-1))
    KmpSearch invokePrivate makeTable("def") should equal(Array(-1,0,0))
    KmpSearch invokePrivate makeTable("aaba") should equal(Array(-1,0,1,0))
    KmpSearch invokePrivate makeTable("aaaa") should equal(Array(-1,0,1,2))
    KmpSearch invokePrivate makeTable("computer") should equal(Array(-1,0,0,0,0,0,0,0))
    KmpSearch invokePrivate makeTable("comcom") should equal(Array(-1,0,0,0,1,2))
  }

  it should "give illegal argument exception for empty string pattern" in {
    assertThrows[IllegalArgumentException](KmpSearch invokePrivate makeTable(""))
  }

  "search method" should "return true, if pattern match in text" in {
    assert(KmpSearch.search("mask","ma" ))
    assert(KmpSearch.search("a","a"))
    assert(KmpSearch.search("video","video"))
  }

  it should "return true, if empty string is given as pattern" in {
    assert(KmpSearch.search("testing",""))
    assert(KmpSearch.search("hello",""))
  }

  it should "return false, if pattern is not matching in text" in {
    assertResult(false)(KmpSearch.search("matcher","ate"))
    KmpSearch.search("testing","get") should be(false)
    KmpSearch.search("testing"," ") should be(false)
  }
}
