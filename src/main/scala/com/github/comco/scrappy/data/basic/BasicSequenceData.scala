package com.github.comco.scrappy.data.basic

import com.github.comco.scrappy._

case class BasicSequenceData[-Source <: Shape.Any, +Element <: Shape.Any](
  val elements: Seq[Data[Source, Element]],
  val origin: Origin[Source, Shape.Sequence[Element]],
  val schema: Schema.Sequence[Element])
    extends Data.RichSequence[Source, Element]