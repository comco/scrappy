package com.github.comco.scrappy.picker

import com.github.comco.scrappy._

case class Tuple1Picker[-Source <: Shape.Any, +Coordinate1 <: Shape.Any](
  val coordinate1Picker: Picker[Source, Coordinate1])(
    implicit schemaFactory: Schema.Factory, dataFactory: Data.Factory)
    extends BasePicker[Source, Shape.Tuple1[Coordinate1]] {

  override def sourceSchema = coordinate1Picker.sourceSchema

  override def targetSchema = Schema.Tuple(coordinate1Picker.targetSchema)

  override def doPick(source: Data[Source]) =
    Data.Tuple(coordinate1Picker.pick(source))(source.origin.computed, targetSchema)
}