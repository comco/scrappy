package com.github.comco.scrappy.picker

import com.github.comco.scrappy._

case class SelfPicker[-Source <: Shape.Any, Shape <: Shape.Any](val sourceSchema: Schema[Shape])
    extends BasePicker[Shape, Shape] {

  override def targetSchema = sourceSchema

  override def doPick(source: Data[Source, Shape]) = source
}