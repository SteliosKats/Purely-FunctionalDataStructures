package homegrown.collections

trait Set[Element] extends (Element => Boolean) {
  import Set._

  final override def apply(input: Element): Boolean =
    contains(input)

  final def contains(input: Element): Boolean =
    exists(_ == input)

  final def doesNotContain(input: Element): Boolean =
    !contains(input)

  final def exists(predicate: Element => Boolean): Boolean = fold(false)((acc, current) => acc || predicate(current))

  final def doesNotExist(predicate: Element => Boolean): Boolean = !exists(predicate)

  final def forAll(predicate: Element => Boolean): Boolean = fold(true)((acc, current) => acc && predicate(current))

  final def notForAll(predicate: Element => Boolean): Boolean = !forAll(predicate)

  @scala.annotation.tailrec
  final def fold[Result](init: Result)(function: (Result, Element) => Result): Result = {
    if (isEmpty)
      init
    else {
      val nonEmptySet = this.asInstanceOf[NonEmpty[Element]]
      val element = nonEmptySet.element
      val otherElements = nonEmptySet.otherElements
      otherElements.fold(function(init, element))(function)
    }
  }

  final def add(input: Element): Set[Element] =
    fold(NonEmpty(input, empty)) {
      (acc, currElement) =>
        if (currElement == input) acc else NonEmpty(currElement, acc)
    }

  final def remove(input: Element): Set[Element] =
    fold(empty[Element]) {
      (acc, currElement) => if (currElement == input) acc else NonEmpty(currElement, acc)
    }

  final def union(that: Set[Element]): Set[Element] =
    fold(that)(_.add(_))

  final def filter(predicate: Element => Boolean): Set[Element] = fold(empty[Element]) {
    (acc, current) => if (predicate(current)) acc.add(current) else acc
  }

  final def intersection(predicate: Element => Boolean): Set[Element] = filter(predicate)

  final def diff(that: Set[Element]): Set[Element] =
    fold(empty[Element]) { (acc, currElement) => if (!that(currElement)) acc.add(currElement) else acc }

  final def isSubsetOf(predicate: Element => Boolean): Boolean =
    forAll(predicate)

  final def isSuperSetOf(that: Set[Element]): Boolean = that.isSubsetOf(this)

  final override def equals(other: Any): Boolean = other match {
    case that: Set[Element] => this.isSubsetOf(that) && that.isSubsetOf(this)
    case _                  => false
  }

  final def isEmpty: Boolean = {
    this.isInstanceOf[Empty[Element]]
  }

  final def size: Int = {
    fold(0) { (curr, _) => curr + 1 }
  }

  final def isSingleton: Boolean = {
    if (isEmpty)
      false
    else {
      val nonEmptySet = this.asInstanceOf[NonEmpty[Element]]
      val element = nonEmptySet.element
      val otherElements = nonEmptySet.otherElements
      otherElements.isEmpty
    }
  }

  final def foreach[Result](function: Element => Result): Unit = {
    fold(()) {
      (acc, current) => function(current)
    }
  }

  final override def hashCode: Int = fold(41)(_ + _.hashCode)

  final def map[Result](function: Element => Result): Set[Result] = fold(empty[Result])((acc, element) => acc.add(function(element)))

  final def flatMap[Result](function: Element => Set[Result]): Set[Result] =
    fold(empty[Result]) {
      (outerAcc, outerCurr) =>
        function(outerCurr).fold(outerAcc) {
          (innerAcc, innerCurr) => innerAcc.add(innerCurr)
        }
    }

  final def nonEmpty: Boolean = !isEmpty

}

object Set {
  def apply[Element](element: Element, otherElements: Element*): Set[Element] = otherElements.foldLeft(empty[Element].add(element))((acc, currentEl) => acc.add(currentEl))

  private final case class NonEmpty[Element](element: Element, otherElements: Set[Element]) extends Set[Element]

  private object NonEmpty {
    private[this] def unapply(any: Any): Option[(String, Any)] = patternMatchingNotSupported
  }

  private class Empty[Element] extends Set[Element] {
    private[this] def unapply(any: Any): Option[(String, Any)] = patternMatchingNotSupported
  }

  private[this] def unapply(any: Any): Option[(String, Any)] = patternMatchingNotSupported

  private[this] def patternMatchingNotSupported: Nothing = sys.error("pattern matching on Sets is expensive and therefore not supported")

  def empty[Element]: Set[Element] = new Empty[Element]
}
