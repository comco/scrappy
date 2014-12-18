package com.github.comco.scrappy.originated_data

import com.github.comco.scrappy.origin.Origin
import com.github.comco.scrappy.data.TupleData
import com.github.comco.scrappy.originated_data.OriginatedData
import com.github.comco.scrappy.originated_data.OriginatedTupleData
import com.github.comco.scrappy.pointer.CoordinateStep

case class SimpleOriginalTupleData(val data: TupleData, val origin: Origin)
    extends OriginatedTupleData {

  lazy val coordinates = data.coordinates.zipWithIndex.map {
    case (coord, pos) =>
      val coordOrigin = origin.append(CoordinateStep(datatype, pos))
      OriginatedData.from(coord, coordOrigin)
  }
}
