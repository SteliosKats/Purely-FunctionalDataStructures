package homegrown.collections

trait Set extends (String => Boolean) {
  def add(input: String): Set = element => element == input || this(element)
  def remove(input: String): Set = element => element != input || this(element)
}

object Set {
  def empty: Set = input => false
}
