package com.github.comco.scrappy.picker

import com.github.comco.scrappy._

case class Tuple2Picker[-Source <: Shape.Any, +Coordinate1 <: Shape.Any, +Coordinate2 <: Shape.Any](
  val coordinate1Picker: Picker[Source, Coordinate1],
  val coordinate2Picker: Picker[Source, Coordinate2])(
    implicit schemaFactory: Schema.Factory, dataFactory: Data.Factory)
    extends BasePicker[Source, Shape.Tuple2[Coordinate1, Coordinate2]] {

  override def sourceSchema = coordinate1Picker.sourceSchema

  override def targetSchema = {
    Schema.Tuple(
      coordinate1Picker.targetSchema,
      coordinate2Picker.targetSchema)
  }

  override def doPick(source: Data[Source]) = {
    Data.Tuple(
      coordinate1Picker.pick(source),
      coordinate2Picker.pick(source))(
        source.origin.computed,
        targetSchema)
  }
}