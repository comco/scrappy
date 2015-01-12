package com.github.comco.scrappy.picker

import com.github.comco.scrappy.Shape
import com.github.comco.scrappy.Picker
import com.github.comco.scrappy.Data

case class AndThenPicker[-Source <: Shape.Any, Middle <: Shape.Any, +Target <: Shape.Any](
  val first: Picker[Source, Middle], val second: Picker[Middle, Target])
    extends BasePicker[Source, Target] {
  require(first.targetSchema.satisfies(second.sourceSchema),
    s"Pickers ${first} and ${second} should be compatible.")

  override def sourceSchema = first.sourceSchema

  override def targetSchema = second.targetSchema

  override def doPick(source: Data[Source]): Data[Target] = {
    second.pick(first.pick(source))
  }
}