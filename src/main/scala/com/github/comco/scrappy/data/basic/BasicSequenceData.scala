package com.github.comco.scrappy.data.basic

import com.github.comco.scrappy._

case class BasicSequenceData[+Element <: Shape.Any](
  val schema: Schema.Sequence[Element],
  val origin: Origin.Sequence[Element],
  val elements: Seq[Data[Element]])
    extends Data.RichSequence[Element]