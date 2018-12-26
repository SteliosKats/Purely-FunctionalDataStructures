package homegrown.collections

trait Set extends (String => Boolean) {
  final def add(input: String): Set = element => element == input || this(element)
  final def remove(input: String): Set = element => input != element || !this(element)
  final def union(that: Set): Set = element => this(element) || that(element)

}

object Set {
  def empty: Set = input => false
}

