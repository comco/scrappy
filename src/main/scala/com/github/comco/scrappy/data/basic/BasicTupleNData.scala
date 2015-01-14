package com.github.comco.scrappy.data.basic

import com.github.comco.scrappy._

case class BasicTupleNData(
  val schema: Schema.Tuple,
  val origin: Origin.Tuple,
  val coordinates: IndexedSeq[Data[Shape.Any]])
    extends Data.RichTuple