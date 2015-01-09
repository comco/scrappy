package com.github.comco.scrappy.schema.basic

import com.github.comco.scrappy.Shape
import com.github.comco.scrappy.Schema

case class BasicSequenceSchema[+Element <: Shape.Any](val elementSchema: Schema[Element]) extends Schema.RichSequence[Element]