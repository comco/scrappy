package com.github.comco.scrappy.picker

import com.github.comco.scrappy.data.Data
import com.github.comco.scrappy.originated_data.OriginatedData

/**
 * Base class for general pickers.
 */
abstract class BasePicker extends Picker {
  def pickData(source: Data) = {
    require(source.datatype.isSubtypeOf(sourceType))
    doPickData(source)
  } ensuring (_.datatype.isSubtypeOf(targetType))

  def doPickData(source: Data): Data

  def pickOriginatedData(source: OriginatedData) = {
    require(source.datatype.isSubtypeOf(sourceType))
    doPickOriginatedData(source)
  } ensuring (_.datatype.isSubtypeOf(targetType))

  def doPickOriginatedData(source: OriginatedData): OriginatedData
}