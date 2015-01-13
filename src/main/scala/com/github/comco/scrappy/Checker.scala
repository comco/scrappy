package com.github.comco.scrappy

abstract class Checker[Shape <: Shape.Any] {
  def sourceSchema: Schema.Any

  def check(source: Data[Shape]): Reason.Result[Shape]
}