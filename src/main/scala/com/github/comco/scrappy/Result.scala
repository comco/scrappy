package com.github.comco.scrappy

sealed abstract class Reason[-Source <: Shape.Any, +Target <: Shape.Any]

object Reason {
  case class Result[-Source <: Shape.Any, +Target <: Shape.Any](
    val successful: Boolean,
    val scope: Origin[Target],
    val reasons: Set[Reason[Source, Target]],
    val checker: Checker[Source])
      extends Reason

  case class Witness[+Target <: Shape.Any](
    val witness: Origin[Target]) extends Reason
}
