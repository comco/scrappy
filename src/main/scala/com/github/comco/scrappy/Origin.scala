package com.github.comco.scrappy

/**
 * Representation of the origin of a value - either it is original or computed.
 */
sealed abstract class Origin {
  def append(step: Step): Origin
  
  def computed: Origin = ???
}

case class Original(val pointer: Pointer) extends Origin {
  def append(step: Step) = ???
}

case class Computed(val pointers: Set[Pointer]) extends Origin {
  def append(step: Step) = this
}