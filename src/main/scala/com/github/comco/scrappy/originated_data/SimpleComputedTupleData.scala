package com.github.comco.scrappy.originated_data

import com.github.comco.scrappy.origin.Origin
import com.github.comco.scrappy.data.TupleData
import com.github.comco.scrappy.originated_data.OriginatedData
import com.github.comco.scrappy.originated_data.OriginatedTupleData

case class SimpleComputedTupleData(
  val data: TupleData,
  val origin: Origin,
  val coordinates: IndexedSeq[OriginatedData])
    extends OriginatedTupleData