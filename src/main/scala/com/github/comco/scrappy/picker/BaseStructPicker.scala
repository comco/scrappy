package com.github.comco.scrappy.picker

import com.github.comco.scrappy.StructType
import com.github.comco.scrappy.data.Data
import com.github.comco.scrappy.data.StructData
import com.github.comco.scrappy.originated_data.OriginatedData
import com.github.comco.scrappy.originated_data.OriginatedStructData

/**
 * Base class for pickers on structs.
 */
abstract class BaseStructPicker extends Picker {
  def sourceType: StructType

  def pickData(source: Data) = {
    require(source.datatype == sourceType)
    doPickData(source.asInstanceOf[StructData])
  } ensuring (_.datatype == targetType)

  def doPickData(source: StructData): Data

  def pickOriginatedData(source: OriginatedData) = {
    require(source.datatype == sourceType)
    doPickOriginatedData(source.asInstanceOf[OriginatedStructData])
  } ensuring (_.datatype == targetType)

  def doPickOriginatedData(source: OriginatedStructData): OriginatedData
}