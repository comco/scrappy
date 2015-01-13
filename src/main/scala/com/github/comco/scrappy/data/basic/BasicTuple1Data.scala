package com.github.comco.scrappy.data.basic

import com.github.comco.scrappy._

case class BasicTuple1Data[+Coordinate1 <: Shape.Any](
  val coordinate1: Data[Coordinate1],
  val origin: Origin.Tuple1[Coordinate1],
  val schema: Schema.Tuple1[Coordinate1])
    extends Data.RichTuple1[Coordinate1]