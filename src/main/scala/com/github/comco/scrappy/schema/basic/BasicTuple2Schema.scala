package com.github.comco.scrappy.schema.basic

import com.github.comco.scrappy.Shape
import com.github.comco.scrappy.Schema

case class BasicTuple2Schema[+Coordinate1 <: Shape.Any, +Coordinate2 <: Shape.Any](val coordinate1Schema: Schema[Coordinate1], val coordinate2Schema: Schema[Coordinate2]) extends Schema.RichTuple2[Coordinate1, Coordinate2]