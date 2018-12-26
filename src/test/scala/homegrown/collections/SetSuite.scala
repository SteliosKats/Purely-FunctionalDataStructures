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

    val newSet2 = set(first)
    //newSet(first) shouldBe true
    set(second) shouldBe true
  }

  test("remove from an Non Empty set should yield an Empty Set") {
    val first: String = generateRandomStr
    val addFirstNonEmpty = Set.empty.add(first)
    addFirstNonEmpty(first) shouldBe true
  }

  test("remove on a non empty Set should yield a new Set without the element") {
    val element = generateRandomStr
    val setWithelement = Set.empty.add(element)
    setWithelement(element) shouldBe true

    val setWithoutElement = setWithelement.remove(element)
    setWithoutElement(element) shouldBe false
  }

  test("remove on a non empty Set should yield a new Set without the element2") {
    val element = generateRandomStr
    val element2 = generateRandomStr
    element should not be element2
    val setWithelement = Set.empty.add(element)
    val setWithelement2 = setWithelement.add(element2)
    setWithelement2(element) shouldBe true

    val setWithoutElement2 = setWithelement2.remove(element2)
    setWithoutElement2(element2) shouldBe false
  }

  test("Union on an empty set should yield an empty Set") {
    val element1 = generateRandomStr
    val element2 = generateRandomStr
    element1 should not be element2
    val emptySet = Set.empty
    val nonEmptySet = emptySet.add(element1).add(element2)
    emptySet.union(nonEmptySet)(element1) shouldBe true
    emptySet.union(nonEmptySet)(element2) shouldBe true

    nonEmptySet.union(emptySet)(element1) shouldBe true
    nonEmptySet.union(emptySet)(element2) shouldBe true
  }

  def generateRandomStr: String = scala.util.Random.alphanumeric.take(7).mkString
}
