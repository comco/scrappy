package com.github.comco.scrappy

sealed abstract class Origin[+Shape <: Shape.Any] {
  def computed: Origin.Computed
}

object Origin extends Domain {
  type Abstract[+Shape <: Shape.Any] = Origin[Shape]

  abstract class Bare extends Nil

  object Bare extends Bare {
    override def computed = Computed(Set.empty)
  }

  abstract class Original[-Source <: Shape.Any, +Target <: Shape.Any](
      val pointer: Pointer[Source, Target]) extends Origin[Target] {
    override def computed = Computed(Set(pointer))
  }

  case class Computed(val pointers: Set[Pointer[Nothing, Shape.Any]])
      extends Nil {
    override def computed = this
  }
}