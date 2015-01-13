package com.github.comco.scrappy.checker

import com.github.comco.scrappy._

abstract class BaseChecker[Shape <: Shape.Any]
    extends Checker[Shape] {
  def doCheck(source: Data[Shape]): Result[Shape]

  override def check(source: Data[Shape]): Result[Shape] = {
    require(source.schema.satisfies(sourceSchema))
    doCheck(source)
  }
}