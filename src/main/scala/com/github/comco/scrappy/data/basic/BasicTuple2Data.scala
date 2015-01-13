package com.github.comco.scrappy.data.basic

import com.github.comco.scrappy._

case class BasicTuple2Data[+Coordinate1 <: Shape.Any, +Coordinate2 <: Shape.Any](
  val coordinate1: Data[Coordinate1],
  val coordinate2: Data[Coordinate2],
  val origin: Origin.Tuple2[Coordinate1, Coordinate2],
  val schema: Schema.Tuple2[Coordinate1, Coordinate2])
    extends Data.RichTuple2[Coordinate1, Coordinate2]