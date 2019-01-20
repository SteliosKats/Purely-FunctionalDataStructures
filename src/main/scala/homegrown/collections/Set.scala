package homegrown.collections

trait Set[Element] extends (Element => Boolean) {
  import Set._
  final override def apply(input: Element): Boolean = {
    var result = false
    foreach {
      current => result = result || current == input
    }
    result
  }
  final def add(input: Element): Set[Element] = {
    var result = NonEmpty(input, empty)
    foreach {
      current => if (current != input) result = NonEmpty(current, result)
    }
    result
  }
  final def remove(input: Element): Set[Element] = {
    var result = Set.empty[Element]
    foreach {
      current => if (input != current) result = NonEmpty(current, result)
    }
    result
  }
  final def union(that: Set[Element]): Set[Element] = {
    var result = this
    that.foreach {
      current => result = result.add(current)
    }
    result
  }
  // } otherElements.union(that.add(element))

  final def intersection(that: Set[Element]): Set[Element] = {
    var result = empty[Element]
    foreach {
      current => if (that(current)) result = result.add(current)
    }
    result
  }

  final def diff(that: Set[Element]): Set[Element] = {
    var result = empty[Element]
    foreach {
      current => if (!that(current)) result = result.add(current)
    }
    result
  }
  def isSubsetOf(that: Set[Element]): Boolean = {
    var result = true
    foreach {
      current => if (!that(current)) result = false
    }
    result
  }
  final def isSuperSetOf(that: Set[Element]): Boolean = that.isSubsetOf(this)
  final override def equals(other: Any): Boolean = other match {
    case that: Set[Element] => this.isSubsetOf(that) && that.isSubsetOf(this)
    case _                  => false
  }

  final def isEmpty: Boolean = {
    this.isInstanceOf[Empty[Element]]
  }

  final def size: Int = {
    var result = 0
    foreach {
      _ => result = result + 1
    }
    result
  }
  def isSingleton: Boolean = {
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
    if (nonEmpty) {
      val nonEmptySet = this.asInstanceOf[NonEmpty[Element]]
      val element = nonEmptySet.element
      val otherElements = nonEmptySet.otherElements
      function(element)
      otherElements.foreach(function)
    }
  }

  final def map[Result](function: Element => Result): Set[Result] = {
    var result = empty[Result]
    foreach {
      current => result = result.add(function(current))
    }
    result
  }

  final def flatMap[Result](function: Element => Set[Result]): Set[Result] = {
    var result = empty[Result]
    foreach {
      outerCurrent =>
        function(outerCurrent).foreach { innerCurrent => result = result.add(innerCurrent)

        }
    }
    result
  }

  final def nonEmpty: Boolean = !isEmpty

}

object Set {
  def apply[Element](element: Element, otherElements: Element*): Set[Element] = {
    var result: Set[Element] = empty[Element].add(element)
    otherElements.foreach { currrent => result = result.add(currrent) }
    result
  }

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
