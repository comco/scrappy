package com.github.comco.scrappy.schema.basic

import com.github.comco.scrappy.Shape
import com.github.comco.scrappy.Schema

case class BasicSomeSchema[+Value <: Shape.Concrete](val valueSchema: Schema[Value]) extends Schema.RichSome[Value]