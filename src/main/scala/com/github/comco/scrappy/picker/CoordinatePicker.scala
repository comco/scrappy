package com.github.comco.scrappy.picker

import com.github.comco.scrappy.TupleType
import com.github.comco.scrappy.data.TupleData
import com.github.comco.scrappy.originated_data.OriginatedTupleData

/**
 * Picker for a coordinate of a tuple.
 */
case class CoordinatePicker(val sourceType: TupleType, val position: Int)
    extends BaseTuplePicker {
  require(sourceType.hasCoordinate(position),
    s"TupleType: $sourceType does not have a coordinate at position: $position")

  def targetType = sourceType.coordinateType(position)

  def doPickData(source: TupleData) = source.coordinates(position)
  def doPickOriginatedData(source: OriginatedTupleData) = source.coordinates(position)
}
