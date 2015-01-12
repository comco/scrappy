package com.github.comco.scrappy.data.basic

import com.github.comco.scrappy._

case class BasicTuple2Data[-Source <: Shape.Any, +Coordinate1 <: Shape.Any, +Coordinate2 <: Shape.Any](
  val coordinate1: Data[Source, Coordinate1],
  val coordinate2: Data[Source, Coordinate2],
  val origin: Origin[Source, Shape.Tuple2[Coordinate1, Coordinate2]],
  val schema: Schema.Tuple2[Coordinate1, Coordinate2])
    extends Data.RichTuple2[Source, Coordinate1, Coordinate2]