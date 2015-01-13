package com.github.comco.scrappy.checker

import com.github.comco.scrappy._

/*
 *  First picks data, then checks it.
 */
case class PickingChecker[Source <: Shape.Any, Target <: Shape.Any](
  val picker: Picker[Source, Target],
  val checker: Checker[Target])
    extends BaseChecker[Source] {
  require(picker.targetSchema.satisfies(checker.sourceSchema))

  override def sourceSchema = picker.sourceSchema

  override def doCheck(source: Data[Source]): Result[Source] = {
    val target = picker.pick(source)
    val result = checker.check(target)
    return Result(result.successful, source.origin, Set(result), this)
  }
}