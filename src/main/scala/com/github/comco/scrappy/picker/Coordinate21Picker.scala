package com.github.comco.scrappy.picker

import com.github.comco.scrappy._

case class Coordinate21Picker[Coordinate1 <: Shape.Any](
  val sourceSchema: Schema.Tuple2[Coordinate1, Nothing])
    extends BasePicker[Shape.Tuple2[Coordinate1, Nothing], Coordinate1] {

  override def targetSchema = sourceSchema.coordinate1Schema

  override def doPick(source: Data.Tuple2[Coordinate1, Nothing]): Data[Coordinate1] =
    source.coordinate1
}