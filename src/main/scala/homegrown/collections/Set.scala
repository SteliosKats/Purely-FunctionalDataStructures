package homegrown.collections

trait Set extends (String => Boolean) {
  def add(input: String): Set
  def remove(input: String): Set
  def union(that: Set): Set
  def intersection(that: Set): Set
  def diff(that: Set): Set
  /*
  final def union(that: Set): Set = element => this(element) || that(element)
  final def intersection(that: Set): Set = element => this(element) && that(element)
  final def diff(that: Set): Set = element => this(element) && !that(element)*/
}

object Set {
  private final case class NonEmpty(element: String, otherElements: Set) extends Set {
    def apply(input: String): Boolean = input == element || otherElements(input)
    final override def add(input: String): Set = if (input == element) this else NonEmpty(input, otherElements.add(element))
    final override def remove(input: String): Set = if (input == element) otherElements else NonEmpty(element, otherElements.remove(input))
    final override def union(that: Set): Set = otherElements.union(that.add(element)) //TODO check if otherElements.union(NonEmpty(element, that)) is also true
    final override def intersection(that: Set): Set = if (that(element)) otherElements.intersection(that).add(element) else otherElements.intersection(that)
    final override def diff(that: Set): Set = if (that(element)) otherElements.diff(that) else otherElements.diff(that).add(element)
  }
  private object Empty extends Set {
    def apply(input: String): Boolean = false
    final override def add(input: String): Set = NonEmpty(input, Empty)
    final override def remove(input: String): Set = this //or Empty
    final override def union(that: Set): Set = that
    final override def intersection(that: Set): Set = this //or Empty
    final override def diff(that: Set): Set = this
  }

  def empty: Set = Empty
}
