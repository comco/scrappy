package com.github.comco.scrappy.data.basic

import com.github.comco.scrappy._

case class BasicTupleNData[-Source <: Shape.Any](
  val coordinates: IndexedSeq[Data[Source, Shape.Any]],
  val origin: Origin[Source, Shape.Tuple],
  val schema: Schema.Tuple)
    extends Data.RichTuple[Source]