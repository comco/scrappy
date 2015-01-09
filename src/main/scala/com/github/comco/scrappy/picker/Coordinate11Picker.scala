package com.github.comco.scrappy.picker

import com.github.comco.scrappy._

case class Coordinate11Picker[Coordinate1 <: Shape.Any](
  val sourceSchema: Schema.Tuple1[Coordinate1])
    extends BasePicker[Shape.Tuple1[Coordinate1], Coordinate1] {

  override def targetSchema = sourceSchema.coordinate1Schema

  override def doPick(source: Data.Tuple1[Coordinate1]): Data[Coordinate1] =
    source.coordinate1
}