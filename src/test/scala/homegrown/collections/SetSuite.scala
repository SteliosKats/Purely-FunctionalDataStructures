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
    val addFirstNonEmpty = OldSet.empty.add(first)
    addFirstNonEmpty(first) shouldBe true
  }

  test("remove on a non empty Set should yield a new Set without the element") {
    val element = generateRandomStr
    val setWithelement = OldSet.empty.add(element)
    setWithelement(element) shouldBe true

    val setWithoutElement = setWithelement.remove(element)
    setWithoutElement(element) shouldBe false
  }

  test("remove on a non empty Set should yield a new Set without the element2") {
    val element = generateRandomStr
    val element2 = generateRandomStr
    element should not be element2
    val setWithelement = OldSet.empty.add(element)
    val setWithelement2 = setWithelement.add(element2)
    setWithelement2(element) shouldBe true

    val setWithoutElement2 = setWithelement2.remove(element2)
    setWithoutElement2(element2) shouldBe false
  }

  test("Union on an empty set should yield an empty Set") {
    val element1 = generateRandomStr
    val element2 = generateRandomStr
    element1 should not be element2
    val emptySet = OldSet.empty
    val nonEmptySet = emptySet.add(element1).add(element2)
    emptySet.union(nonEmptySet)(element1) shouldBe true
    emptySet.union(nonEmptySet)(element2) shouldBe true

    nonEmptySet.union(emptySet)(element1) shouldBe true
    nonEmptySet.union(emptySet)(element2) shouldBe true
  }

  test("Union on Sets with differnet elements should yield an non empty Set with their elements combined") {
    val el1 = generateRandomStr
    val el2 = generateRandomStr
    val el3 = generateRandomStr
    val el4 = generateRandomStr

    el1 should not be el3
    el1 should not be el4
    el2 should not be el3
    el2 should not be el4
    val nonEmptySet1 = OldSet.empty.add(el1).add(el2)
    val nonEmptySet2 = OldSet.empty.add(el3).add(el4)

    nonEmptySet1.union(nonEmptySet2)(el1) shouldBe true
    nonEmptySet1.union(nonEmptySet2)(el2) shouldBe true
    nonEmptySet2.union(nonEmptySet1)(el3) shouldBe true
    nonEmptySet2.union(nonEmptySet1)(el4) shouldBe true
  }

  test("Intersection on an empty set should yield an empty Set") {
    val element1 = generateRandomStr
    val element2 = generateRandomStr
    element1 should not be element2
    val emptySet = OldSet.empty
    val nonEmptySet = emptySet.add(element1).add(element2)
    emptySet.intersection(nonEmptySet)(element1) shouldBe false
    emptySet.intersection(nonEmptySet)(element2) shouldBe false

    nonEmptySet.intersection(emptySet)(element1) shouldBe false
    nonEmptySet.intersection(emptySet)(element2) shouldBe false
  }

  test("Intersection on Sets with differnet elements should yield an empty Set ") {
    val el1 = generateRandomStr
    val el2 = generateRandomStr
    val el3 = generateRandomStr
    val el4 = generateRandomStr

    el1 should not be el3
    el1 should not be el4
    el2 should not be el3
    el2 should not be el4
    val nonEmptySet1 = OldSet.empty.add(el1).add(el2)
    val nonEmptySet2 = OldSet.empty.add(el3).add(el4)

    nonEmptySet1.intersection(nonEmptySet2)(el1) shouldBe false
    nonEmptySet1.intersection(nonEmptySet2)(el2) shouldBe false
    nonEmptySet2.intersection(nonEmptySet1)(el3) shouldBe false
    nonEmptySet2.intersection(nonEmptySet1)(el4) shouldBe false
  }

  test("Difference on empty Sets should yield an empty Set") {
    val element1 = generateRandomStr
    val element2 = generateRandomStr
    element1 should not be element2
    val emptySet = OldSet.empty
    val emptySet2 = OldSet.empty
    emptySet.diff(emptySet2)(element1) shouldBe false
  }

  test("Difference on an Non-empty set and empty Set should yield an Non-empty Set") {
    val element1 = generateRandomStr
    val element2 = generateRandomStr
    element1 should not be element2
    val emptySet = OldSet.empty
    val nonEmptySet = emptySet.add(element1).add(element2)
    nonEmptySet.diff(emptySet)(element1) shouldBe true
    nonEmptySet.diff(emptySet)(element2) shouldBe true

    emptySet.diff(nonEmptySet)(element1) shouldBe false
    emptySet.diff(nonEmptySet)(element2) shouldBe false
  }

  test("Difference between Non-empty Sets with differnet elements should yield an Non-empty Set with the elements of the first Set") {
    val el1 = generateRandomStr
    val el2 = generateRandomStr
    val el3 = generateRandomStr
    val el4 = generateRandomStr

    el1 should not be el3
    el1 should not be el4
    el2 should not be el3
    el2 should not be el4
    val nonEmptySet1 = OldSet.empty.add(el1).add(el2)
    val nonEmptySet2 = OldSet.empty.add(el3).add(el4)

    nonEmptySet1.diff(nonEmptySet2)(el1) shouldBe true
    nonEmptySet1.diff(nonEmptySet2)(el2) shouldBe true
    nonEmptySet1.diff(nonEmptySet1)(el3) shouldBe false
    nonEmptySet1.diff(nonEmptySet1)(el4) shouldBe false
  }

  def generateRandomStr: String = scala.util.Random.alphanumeric.take(7).mkString
}
