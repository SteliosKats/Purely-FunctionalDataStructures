package homegrown.collections

trait Set extends (String => Boolean) {
  import Set._
  final override def apply(input: String): Boolean = {
    var result = false
    foreach {
      current => result = result || current == input
    }
    result
  }
  final def add(input: String): Set = {
    var result = NonEmpty(input, empty)
    foreach {
      current => if (current != input) result = NonEmpty(current, result)
    }
    result
  }
  final def remove(input: String): Set = {
    var result = Set.empty
    foreach {
      current => if (input != current) result = NonEmpty(current, result)
    }
    result
  }
  def union(that: Set): Set
  def intersection(that: Set): Set
  def diff(that: Set): Set
  def isSubsetOf(that: Set): Boolean
  final def isSuperSetOf(that: Set): Boolean = that.isSubsetOf(this)
  final override def equals(other: Any): Boolean = other match {
    case that: Set => this.isSubsetOf(that) && that.isSubsetOf(this)
    case _         => false
  }
  def isEmpty: Boolean
  def size: Int
  def isSingleton: Boolean
  def foreach(function: String => Unit): Unit

}

object Set {
  def apply(element: String, otherElements: String*): Set = {
    var result: Set = Set.empty.add(element)
    otherElements.foreach { currrent => result = result.add(currrent) }
    result
  }
  private final case class NonEmpty(element: String, otherElements: Set) extends Set {
    final override def union(that: Set): Set = otherElements.union(that.add(element)) //TODO check if otherElements.union(NonEmpty(element, that)) is also true
    final override def intersection(that: Set): Set = if (that(element)) otherElements.intersection(that).add(element) else otherElements.intersection(that)
    final override def diff(that: Set): Set = if (that(element)) otherElements.diff(that) else otherElements.diff(that).add(element)
    final override def isSubsetOf(that: Set): Boolean = if (that(element)) otherElements.isSubsetOf(that) else false
    final override def isEmpty: Boolean = true || otherElements.isEmpty
    final override def size: Int = 1 + otherElements.size
    final override def isSingleton: Boolean = otherElements.isEmpty
    final override def foreach(function: String => Unit): Unit = {
      function(element)
      otherElements.foreach(function)
    }

  }
  private object Empty extends Set {
    final override def union(that: Set): Set = that
    final override def intersection(that: Set): Set = this //or Empty
    final override def diff(that: Set): Set = this
    final override def isSubsetOf(that: Set): Boolean = true
    final override def size: Int = 0
    final override def isSingleton: Boolean = true
    final override def isEmpty = true
    def foreach(function: String => Unit): Unit = ()
  }
  def empty: Set = Empty
}
