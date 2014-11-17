package com.github.comco.scrappy.originated_data.simple

import com.github.comco.scrappy.origin.Origin
import com.github.comco.scrappy.data.TupleData
import com.github.comco.scrappy.originated_data.OriginatedData
import com.github.comco.scrappy.originated_data.OriginatedTupleData
import com.github.comco.scrappy.Type

case class SimpleComputedTupleData(
  val data: TupleData,
  val origin: Origin,
  val coordinates: IndexedSeq[OriginatedData[Type[Any]]])
    extends OriginatedTupleData