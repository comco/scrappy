package com.github.comco.scrappy.picker

import com.github.comco.scrappy.Type
import com.github.comco.scrappy.data.Data
import com.github.comco.scrappy.originated_data.OriginatedData

/**
 * Identity picker - picks itself.
 */
case class SelfPicker(val sourceType: Type) extends Picker {
  def targetType = sourceType

  def pickData(source: Data) = {
    require(source.datatype == sourceType,
        s"SelfPicker: $this doesn't support picking data of type: ${source.datatype}")
    source
  }

  def pickOriginatedData(source: OriginatedData) = source
}