package com.github.comco.scrappy

sealed abstract class Origin[+Target <: Shape.Any] {
  def computed: Origin.Computed
}

object Origin extends Domain {
  type Abstract[+Target <: Shape.Any] = Origin[Target]

  abstract class Bare extends Dynamic

  abstract class Original[+Target <: Shape.Any] extends Origin[Target] {
    def pointer: Pointer[Shape.Any, Target]
  }

  abstract class Computed extends Dynamic {
    def pointers: Set[Pointer.Any]
  }
}