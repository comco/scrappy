package com.github.comco.scrappy.picker

import com.github.comco.scrappy._

abstract class BasePicker[-Source <: Shape.Any, +Target <: Shape.Any]
    extends Picker[Source, Target] {

  def doPick(source: Data[Source]): Data[Target]

  override def pick(source: Data[Source]): Data[Target] = {
    require(source.schema.satisfies(sourceSchema), s"Data $source should satisfy schema: $sourceSchema")
    doPick(source)
  } ensuring ((result: Data[Target]) => targetSchema.satisfies(result.schema),
    s"Resulting data doesn't have schema: $targetSchema.")
}