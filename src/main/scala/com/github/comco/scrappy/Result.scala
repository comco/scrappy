package com.github.comco.scrappy

sealed abstract class Reason

case class Result[Shape <: Shape.Any](
  val successful: Boolean,
  val scope: Origin[Shape],
  val reasons: Set[Reason],
  val checker: Checker[Shape])
    extends Reason

case class Witness[Shape <: Shape.Any](
  val witness: Origin[Shape]) extends Reason