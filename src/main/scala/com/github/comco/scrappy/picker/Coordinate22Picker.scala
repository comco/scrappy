package com.github.comco.scrappy.picker

import com.github.comco.scrappy._

case class Coordinate22Picker[Coordinate2 <: Shape.Any](
  val sourceSchema: Schema.Tuple2[Nothing, Coordinate2])
    extends BasePicker[Shape.Tuple2[Nothing, Coordinate2], Coordinate2] {

  override def targetSchema = sourceSchema.coordinate1Schema

  override def doPick(source: Data.Tuple2[Nothing, Coordinate2]): Data[Coordinate2] =
    source.coordinate1
}