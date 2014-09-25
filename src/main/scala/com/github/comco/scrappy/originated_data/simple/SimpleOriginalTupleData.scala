package com.github.comco.scrappy.originated_data.simple

import com.github.comco.scrappy.CoordinateStep
import com.github.comco.scrappy.Origin
import com.github.comco.scrappy.data.TupleData
import com.github.comco.scrappy.originated_data.OriginatedData
import com.github.comco.scrappy.originated_data.OriginatedTupleData

case class SimpleOriginalTupleData(val data: TupleData, val origin: Origin)
    extends OriginatedTupleData {

  lazy val coordinates = data.coordinates.zipWithIndex.map {
    case (coord, pos) =>
      val coordOrigin = origin.append(CoordinateStep(datatype, pos))
      OriginatedData.from(coord, coordOrigin)
  }
}
