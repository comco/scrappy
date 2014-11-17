package com.github.comco.scrappy.picker

import com.github.comco.scrappy.Type
import com.github.comco.scrappy.data.Data
import com.github.comco.scrappy.originated_data.OriginatedData

/**
 * Identity picker - picks itself.
 */
case class SelfPicker[SourceType >: Type.Nil <: Type.Any](val sourceType: SourceType)
    extends Picker[SourceType, SourceType] {
  def targetType = sourceType

  def pickData(source: Data[SourceType]) = {
    require(source.datatype == sourceType,
      s"SelfPicker: $this doesn't support picking data of type: ${source.datatype}")
    source
  }

  def pickOriginatedData(source: OriginatedData[SourceType]) = {
    require(source.datatype == sourceType,
      s"SelfPicker: $this doesn't support picking originated data of type: ${source.datatype}")
    source
  }
}