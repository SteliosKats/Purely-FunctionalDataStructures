import homegrown.collections._

import org.scalatest._

class TestSuite extends FunSuite with Matchers {
  test("Unit tests for an empty Set") {
    val randomElem = Set.empty(generateRandomStr) shouldBe false

  }

  test("add on an Empty Set should yield a new Set with one element") {
    val first: String = generateRandomStr

    val second: String = generateRandomStr
    first should not be second

    val set = Set.empty.add(first)

    set(first) shouldBe true
    set(second) shouldBe false
  }

  test("add on an non Empty Set should yield a new Set with two elements") {

    val first: String = generateRandomStr

    val second: String = generateRandomStr

    val set = Set.empty.add(first).add(second)
    //val newSet = set.add(second)

    //newSet(first) shouldBe true
    set(second) shouldBe true
  }

  test("remove from an Non Empty set should yield an Empty Set") {
    val first: String = generateRandomStr
    val addFirstNonEmpty = Set.empty.add(first)
    addFirstNonEmpty(first) shouldBe true
  }
  def generateRandomStr: String = scala.util.Random.alphanumeric.take(7).mkString
}
