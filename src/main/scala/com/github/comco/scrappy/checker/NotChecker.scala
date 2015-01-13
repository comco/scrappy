package com.github.comco.scrappy.checker

import com.github.comco.scrappy._

case class NotChecker[Shape <: Shape.Any](
  val checker: Checker[Shape])
    extends BaseChecker[Shape] {
  override def sourceSchema = checker.sourceSchema

  override def doCheck(source: Data[Shape]): Result[Shape] = {
    val result = checker.check(source)
    return Result(!result.successful, source.origin, Set(result), this)
  }
}