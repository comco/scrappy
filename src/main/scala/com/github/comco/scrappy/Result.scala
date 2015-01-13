package com.github.comco.scrappy

sealed abstract class Reason[Shape <: Shape.Any]

object Reason {
  case class Result[Shape <: Shape.Any](
    val successful: Boolean,
    val scope: Origin[Shape],
    val reasons: Set[Reason[Shape]],
    val checker: Checker[Shape])
      extends Reason

  case class Witness[Shape <: Shape.Any](
    val witness: Origin[Shape]) extends Reason
}
