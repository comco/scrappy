package com.github.comco.scrappy.data.simple

import com.github.comco.scrappy.TupleType
import com.github.comco.scrappy.data.Data
import com.github.comco.scrappy.data.TupleData
import com.github.comco.scrappy.Type
import com.github.comco.scrappy.Shape

case class SimpleTupleData(val datatype: TupleType,
  val coordinates: IndexedSeq[Data.Any])
    extends TupleData