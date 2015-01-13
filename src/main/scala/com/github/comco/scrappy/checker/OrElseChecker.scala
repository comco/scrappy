package com.github.comco.scrappy.checker

import com.github.comco.scrappy._

case class OrElseChecker[Shape <: Shape.Any](
  val checker1: Checker[Shape],
  val checker2: Checker[Shape])
    extends BaseChecker[Shape] {
  require(checker1.sourceSchema == checker2.sourceSchema)

  override def sourceSchema = checker1.sourceSchema

  override def doCheck(source: Data[Shape]): Result[Shape] = {
    val result1 = checker1.check(source)
    if (result1.successful) {
      return Result(true, source.origin, Set(result1), this)
    } else {
      val result2 = checker2.check(source)
      if (result2.successful) {
        return Result(true, source.origin, Set(result2), this)
      } else {
        return Result(false, source.origin, Set(result1, result2), this)
      }
    }
  }
}