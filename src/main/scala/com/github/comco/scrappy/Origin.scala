package com.github.comco.scrappy

sealed abstract class Origin[+Shape <: Shape.Any] {
  def computed: Origin.Computed
}

object Origin extends Domain {
  type Abstract[+Shape <: Shape.Any] = Origin[Shape]

  abstract class Bare extends Dynamic

  abstract class Original[-Source <: Shape.Any, +Target <: Shape.Any] extends Origin[Target] {
    def pointer: Pointer[Source, Target]
  }

  abstract class Computed extends Dynamic {
    def pointers: Set[Pointer.Any]
  }
}