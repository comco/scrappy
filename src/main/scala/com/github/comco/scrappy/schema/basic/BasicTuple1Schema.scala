package com.github.comco.scrappy.schema.basic

import com.github.comco.scrappy.Shape
import com.github.comco.scrappy.Schema

case class BasicTuple1Schema[+Coordinate1 <: Shape.Any](val coordinate1Schema: Schema[Coordinate1]) extends Schema.RichTuple1[Coordinate1]