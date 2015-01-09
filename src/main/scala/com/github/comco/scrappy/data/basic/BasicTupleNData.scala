package com.github.comco.scrappy.data.basic

import com.github.comco.scrappy._

case class BasicTupleNData(
  val coordinates: IndexedSeq[Data.Any],
  val origin: Origin.Tuple,
  val schema: Schema.Tuple)
    extends Data.RichTuple