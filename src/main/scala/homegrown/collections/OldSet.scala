package homegrown.collections

trait OldSet extends (String => Boolean) {
  final def add(input: String): OldSet = element => element == input || this(element)
  final def remove(input: String): OldSet = element => input != element || !this(element)
  final def union(that: OldSet): OldSet = element => this(element) || that(element)
  final def intersection(that: OldSet): OldSet = element => this(element) && that(element)
  final def diff(that: OldSet): OldSet = element => this(element) && !that(element)
}

object OldSet {
  def empty: OldSet = input => false
}
