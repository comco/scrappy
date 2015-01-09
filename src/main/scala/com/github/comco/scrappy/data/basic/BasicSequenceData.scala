package com.github.comco.scrappy.data.basic

import com.github.comco.scrappy._

case class BasicSequenceData[+Element <: Shape.Any](
  val elements: Seq[Data[Element]],
  val origin: Origin.Sequence[Element],
  val schema: Schema.Sequence[Element])
    extends Data.RichSequence[Element]