package com.github.comco.scrappy.picker

import com.github.comco.scrappy.StructType
import com.github.comco.scrappy.data.Data
import com.github.comco.scrappy.data.StructData
import com.github.comco.scrappy.originated_data.OriginatedData
import com.github.comco.scrappy.originated_data.OriginatedStructData

/**
 * Base class for pickers on structs.
 */
abstract class BaseStructPicker extends BasePicker {
  def sourceType: StructType

  def doPickData(source: Data) = {
    doPickData(source.asInstanceOf[StructData])
  }

  def doPickData(source: StructData): Data

  def doPickOriginatedData(source: OriginatedData) = {
    doPickOriginatedData(source.asInstanceOf[OriginatedStructData])
  }

  def doPickOriginatedData(source: OriginatedStructData): OriginatedData
}