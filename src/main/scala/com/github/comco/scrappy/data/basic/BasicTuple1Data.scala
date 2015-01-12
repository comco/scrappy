package com.github.comco.scrappy.data.basic

import com.github.comco.scrappy._

case class BasicTuple1Data[-Source <: Shape.Any, +Coordinate1 <: Shape.Any](
  val coordinate1: Data[Source, Coordinate1],
  val origin: Origin[Source, Shape.Tuple1[Coordinate1]],
  val schema: Schema.Tuple1[Coordinate1])
    extends Data.RichTuple1[Source, Coordinate1]