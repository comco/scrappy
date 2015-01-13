package com.github.comco.scrappy

abstract class Checker[+Target <: Shape.Any] {
  def sourceSchema: Schema.Any

  def check[Source <: Shape.Any](source: Data[Target]): Reason.Result[Source, Target]
}