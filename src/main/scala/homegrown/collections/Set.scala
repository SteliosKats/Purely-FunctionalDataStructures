package homegrown.collections

trait Set extends (String => Boolean) {
  def add(input: String): Set = element => element == input || this(element)
  final def remove(input: String): Set = element => input != element && !this(element)
  final def union(that: Set): Set = element => this(element) || that(element)
  final def intersection(that: Set): Set = element => this(element) && that(element)
  final def diff(that: Set): Set = element => this(element) && !that(element)
}

object Set {
  private final case class NonEmpty(element: String, otherElements: Set) extends Set {
    final override def apply(input: String): Boolean = input == element || otherElements(input)
  }
  private object Empty extends Set {
    def apply(input: String): Boolean = false
  }
  def empty: Set = Empty
}
