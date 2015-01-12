package com.github.comco.scrappy

sealed abstract class Origin[-Source <: Shape.Any, +Target <: Shape.Any] {
  def computed: Origin.Computed
}

object Origin {
  type Dynamic = Origin[Shape.Any, Nothing]

  abstract class Bare extends Dynamic

  abstract class Original[-Source <: Shape.Any, +Target <: Shape.Any] extends Origin[Source, Target] {
    def pointer: Pointer[Source, Target]
  }

  abstract class Computed extends Dynamic {
    def pointers: Set[Pointer.Any]
  }
}