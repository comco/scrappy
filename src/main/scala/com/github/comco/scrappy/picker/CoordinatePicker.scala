package com.github.comco.scrappy.picker

import com.github.comco.scrappy.TupleType
import com.github.comco.scrappy.Type
import com.github.comco.scrappy.data.Data
import com.github.comco.scrappy.data.TupleData
import com.github.comco.scrappy.originated_data.OriginatedData
import com.github.comco.scrappy.Shape

/**
 * Picker for a coordinate of a tuple.
 */
case class CoordinatePicker(val sourceType: TupleType, val position: Int)
    extends Picker[Shape.Tuple, Shape.Any] {
  require(sourceType.hasCoordinate(position),
    s"TupleType: $sourceType does not have a coordinate at position: $position")

  def targetType = sourceType.coordinateType(position)

  def doPickData(source: Data.Tuple) = source.coordinates(position)
  def doPickOriginatedData(source: OriginatedData.Tuple) = source.coordinates(position)
}
