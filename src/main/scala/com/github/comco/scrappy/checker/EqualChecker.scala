package com.github.comco.scrappy.checker

import com.github.comco.scrappy._

case class EqualChecker[Source <: Shape.Any, Target <: Shape.Any](
  val picker1: Picker[Source, Target],
  val picker2: Picker[Source, Target])
    extends BaseChecker[Source] {
  require(picker1.sourceSchema == picker2.sourceSchema)
  require(picker1.targetSchema == picker2.targetSchema)

  override def sourceSchema = picker1.sourceSchema

  override def doCheck(source: Data[Source]): Result[Source] = {
    val result1 = picker1.pick(source)
    val result2 = picker2.pick(source)
    val equal = result1.structurallyEquals(result2)
    val witness1 = Witness(result1.origin)
    val witness2 = Witness(result2.origin)
    return Result(equal, source.origin, Set(witness1, witness2), this)
  }
}