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
  final def union(that: Set): Set = {
    var result = this
    that.foreach {
      current => result = result.add(current)
    }
    result
  }
  // } otherElements.union(that.add(element))

  final def intersection(that: Set): Set = {
    var result = empty
    foreach {
      current => if (that(current)) result = result.add(current)
    }
    result
  }

  final def diff(that: Set): Set = {
    var result = empty
    foreach {
      current => if (!that(current)) result = result.add(current)
    }
    result
  }
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
    final override def isSubsetOf(that: Set): Boolean = true
    final override def size: Int = 0
    final override def isSingleton: Boolean = true
    final override def isEmpty = true
    def foreach(function: String => Unit): Unit = ()
  }
  def empty: Set = Empty
}
