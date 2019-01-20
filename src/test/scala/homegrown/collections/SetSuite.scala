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

    val set = Set(first)

    set(first) shouldBe true
    set(second) shouldBe false
  }

  test("add on an non Empty Set should yield a new Set with two elements") {

    val first: String = generateRandomStr

    val second: String = generateRandomStr

    val set = Set(first, second)
    //val newSet = set.add(second)

    val newSet2 = set(first)
    //newSet(first) shouldBe true
    set(second) shouldBe true
  }

  test("remove from an Non Empty set should yield an Empty Set") {
    val first: String = generateRandomStr
    val addFirstNonEmpty = Set(first)
    addFirstNonEmpty(first) shouldBe true
  }

  test("remove on a non empty Set should yield a new Set without the element") {
    val element = generateRandomStr
    val setWithelement = Set(element)
    setWithelement(element) shouldBe true

    val setWithoutElement = setWithelement.remove(element)
    setWithoutElement(element) shouldBe false
  }

  test("remove on a non empty Set should yield a new Set without the element2") {
    val element = generateRandomStr
    val element2 = generateRandomStr
    element should not be element2
    val setWithelement = Set(element)
    val setWithelement2 = setWithelement.add(element2)
    setWithelement2(element) shouldBe true

    val setWithoutElement2 = setWithelement2.remove(element2)
    setWithoutElement2(element2) shouldBe false
  }

  test("Add/remove combo should ensure that all elements are distinct") {
    val element = generateRandomStr

    val set = Set(element, element).remove(element)
    set(element) shouldBe false
  }

  test("Union on an empty set should yield an empty Set") {
    val element1 = generateRandomStr
    val element2 = generateRandomStr
    element1 should not be element2
    val emptySet = Set.empty[String]
    val nonEmptySet = Set[String](element1, element2)
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
    val nonEmptySet1 = Set(el1, el2)
    val nonEmptySet2 = Set(el3, el4)

    nonEmptySet1.union(nonEmptySet2)(el1) shouldBe true
    nonEmptySet1.union(nonEmptySet2)(el2) shouldBe true
    nonEmptySet2.union(nonEmptySet1)(el3) shouldBe true
    nonEmptySet2.union(nonEmptySet1)(el4) shouldBe true
  }

  test("Intersection on an empty set should yield an empty Set") {
    val element1 = generateRandomStr
    val element2 = generateRandomStr
    element1 should not be element2
    val emptySet = Set.empty[String]
    val nonEmptySet: Set[String] = emptySet.add(element1).add(element2)
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
    val nonEmptySet1 = Set.empty.add(el1).add(el2)
    val nonEmptySet2 = Set.empty.add(el3).add(el4)

    nonEmptySet1.intersection(nonEmptySet2)(el1) shouldBe false
    nonEmptySet1.intersection(nonEmptySet2)(el2) shouldBe false
    nonEmptySet2.intersection(nonEmptySet1)(el3) shouldBe false
    nonEmptySet2.intersection(nonEmptySet1)(el4) shouldBe false
  }

  test("Difference on empty Sets should yield an empty Set") {
    val element1 = generateRandomStr
    val element2 = generateRandomStr
    element1 should not be element2
    val emptySet = Set.empty[String]
    val emptySet2 = Set.empty[String]
    emptySet.diff(emptySet2)(element1) shouldBe false
  }

  test("Difference on an Non-empty set and empty Set should yield an Non-empty Set") {
    val element1 = generateRandomStr
    val element2 = generateRandomStr
    element1 should not be element2
    val emptySet = Set.empty[String]
    val nonEmptySet: Set[String] = emptySet.add(element1).add(element2)
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
    val nonEmptySet1 = Set(el1, el2)
    val nonEmptySet2 = Set(el3, el4)

    nonEmptySet1.diff(nonEmptySet2)(el1) shouldBe true
    nonEmptySet1.diff(nonEmptySet2)(el2) shouldBe true
    nonEmptySet1.diff(nonEmptySet1)(el3) shouldBe false
    nonEmptySet1.diff(nonEmptySet1)(el4) shouldBe false
  }

  test("isSubsetOf on itself should yield true") {
    val emptySet = Set.empty
    emptySet.isSubsetOf(emptySet) shouldBe true
  }

  test("isSubsetOf of a Set A with elements of a set B but also different ones of a B ( A <> B ) should yield false") {
    val el1 = generateRandomStr
    val el2 = generateRandomStr
    val el3 = generateRandomStr
    val el4 = generateRandomStr

    val setA = Set(el1, el2, el4)
    val setB = Set(el1, el2, el3)
    setA.isSubsetOf(setB) shouldBe false
  }

  test("isSubsetOf of a Set A with elements of a set B not all elements of B ( A ⊆ B ) should yield true") {
    val el1 = generateRandomStr
    val el2 = generateRandomStr
    val el3 = generateRandomStr
    val el4 = generateRandomStr

    val setA = Set(el1, el2)
    val setB = Set(el1, el2, el3, el4)
    setA.isSubsetOf(setB) shouldBe true
  }

  test("isSubsetOf of a Set A with same and more elements than a set of B ( A ⊇ B ) should yield false") {
    val el1 = generateRandomStr
    val el2 = generateRandomStr
    val el3 = generateRandomStr
    val el4 = generateRandomStr

    val setA = Set.empty.add(el1).add(el2).add(el3).add(el4)
    val setB = Set.empty.add(el1).add(el2)
    setA.isSubsetOf(setB) shouldBe false
  }

  test("isSuperSetOf on itself should yield true") {
    val emptySet = Set.empty
    emptySet.isSuperSetOf(emptySet) shouldBe true
  }

  test("isSuperSetOf of a Set A with elements of a set B but also different ones of a B ( A <> B ) should yield false") {
    val el1 = generateRandomStr
    val el2 = generateRandomStr
    val el3 = generateRandomStr
    val el4 = generateRandomStr

    val setA = Set(el1, el2, el4)
    val setB = Set(el1, el2, el3)
    setA.isSuperSetOf(setB) shouldBe false
  }

  test("isSuperSetOf of a Set A with elements of a set B not all elements of B ( A ⊆ B ) should yield false") {
    val el1 = generateRandomStr
    val el2 = generateRandomStr
    val el3 = generateRandomStr
    val el4 = generateRandomStr

    val setA = Set.empty.add(el1).add(el2)
    val setB = Set.empty.add(el1).add(el2).add(el3).add(el4)
    setA.isSuperSetOf(setB) shouldBe false
  }

  test("isSuperSetOf of a Set A with same and more elements than a set of B ( A ⊇ B ) should yield true") {
    val el1 = generateRandomStr
    val el2 = generateRandomStr
    val el3 = generateRandomStr
    val el4 = generateRandomStr

    val setA = Set.empty.add(el1).add(el2).add(el3).add(el4)
    val setB = Set.empty.add(el1).add(el2).add(el3)
    setA.isSuperSetOf(setB) shouldBe true
  }

  test("Equality of a set with itself should yield true") {
    val emptySet = Set.empty
    emptySet.equals(emptySet) shouldBe true
  }

  test("Equality of a Set A with elements of a set B but also different ones of a B ( A <> B ) should yield false") {
    val el1 = generateRandomStr
    val el2 = generateRandomStr
    val el3 = generateRandomStr
    val el4 = generateRandomStr

    val setA = Set.empty.add(el1).add(el2).add(el4)
    val setB = Set.empty.add(el1).add(el2).add(el3)
    setA.equals(setB) shouldBe false
  }

  test("Equality of a Set A with elements of a set B not all elements of B ( A ⊆ B ) should yield false") {
    val el1 = generateRandomStr
    val el2 = generateRandomStr
    val el3 = generateRandomStr
    val el4 = generateRandomStr

    val setA = Set.empty.add(el1).add(el2)
    val setB = Set.empty.add(el1).add(el2).add(el3).add(el4)
    setA.equals(setB) shouldBe false
  }

  test("Equality of a Set A with same and more elements than a set of B ( A ⊇ B ) should yield false") {
    val el1 = generateRandomStr
    val el2 = generateRandomStr
    val el3 = generateRandomStr
    val el4 = generateRandomStr

    val setA = Set.empty.add(el1).add(el2).add(el3).add(el4)
    val setB = Set.empty.add(el1).add(el2).add(el3)
    setA.equals(setB) shouldBe false
  }

  test("Equality of a Set A with same elements with a set B ( A == B ) should yield true") {
    val el1 = generateRandomStr
    val el2 = generateRandomStr
    val el3 = generateRandomStr
    val el4 = generateRandomStr

    val setA = Set.empty.add(el1).add(el2).add(el3).add(el4)
    val setB = Set.empty.add(el1).add(el2).add(el3).add(el4)
    setA.equals(setB) shouldBe true
  }

  test("foreach on an Empty Set should not apply the function") {
    noException should be thrownBy Set.empty[String].foreach(_ => sys.error("This exception should not be thrown"))
  }

  test("foreach on an non Empty Set should apply the function") {
    var theFunctionWasApplied: Boolean = false
    Set.empty.add(generateRandomStr).foreach(_ => theFunctionWasApplied = true)
    theFunctionWasApplied shouldBe true
  }

  test("foreach shouldBe able to calcuate the size of the given Set 0") {
    var size = 0;
    val set = Set.empty
    set.foreach(_ => size += 1)
    size shouldBe 0
    size shouldBe set.size
  }

  test("foreach shouldBe able to calcuate the size of the given Set 1") {
    var size = 0;
    val set = Set.empty.add(generateRandomStr)
    set.foreach(_ => size += 1)
    size shouldBe 1
    size shouldBe set.size
  }

  test("foreach shouldBe able to calcuate the size of the given Set 4") {
    var size = 0;
    val set = Set.empty.add(generateRandomStr).add(generateRandomStr).add(generateRandomStr).add(generateRandomStr)
    set.foreach(_ => size += 1)
    size shouldBe 4
    size shouldBe set.size
  }

  test("foreach shouldBe able to calcuate the size of the given Set ") {
    var size = 0;
    val randomEl = generateRandomStr
    val set = Set.empty.add(randomEl).add(randomEl).add(randomEl).add(randomEl)
    set.foreach(_ => size += 1)
    size shouldBe 1
    size shouldBe set.size
  }

  test("Set.empty should not compile") {
    "Set()" shouldNot compile
  }

  test("For each should be parametrized in the result of the argument function so that it does not produce warnings") {
    Set.empty[String].foreach(_ => 1)
  }

  test("Map should produce a Set") {
    Set("Hello", "World").map(_.reverse) shouldBe Set("dlroW", "olleH")
  }

  test("Map should be able to produce a Set other than a String") {
    Set("random", "generics", "example").map(_.size) shouldBe Set(6, 8, 7)
  }

  test("FlapMap implementation basic testing") {
    val characters: Set[Char] = Set('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h')
    val numbers: Set[Int] = Set(1, 2, 3, 4, 5, 6, 7, 8)

    val chessboard: Set[(Char, Int)] =
      characters.flatMap { c =>
        numbers.map { n =>
          (c -> n)
        }
      }

    chessboard.foreach(println)
    chessboard.size shouldBe 64
  }
  /*  ignore("calling the varargs aplly method on Set should yield a Set with all the arguments as elements") {
    val el1 = generateRandomStr
    val el2 = generateRandomStr
    val el3 = generateRandomStr
    val el4 = generateRandomStr
    Set(el1, el2, el3, el4) shouldBe Set.empty.add(el1).add(el2).add(el3).add(el4)
  }*/
  def generateRandomStr: String = scala.util.Random.alphanumeric.take(7).mkString
}
