package com.github.comco.scrappy.picker

import com.github.comco.scrappy.Type
import com.github.comco.scrappy.data.Data
import com.github.comco.scrappy.originated_data.OriginatedData

/**
 * Identity picker - picks itself.
 */
case class SelfPicker[A](val sourceType: Type[A]) extends Picker[Type[A], Type[A]] {
  def targetType = sourceType

  def pickData(source: Data[Type[A]]) = {
    require(source.datatype == sourceType,
      s"SelfPicker: $this doesn't support picking data of type: ${source.datatype}")
    source
  }

  def pickOriginatedData(source: OriginatedData[Type[A]]) = {
    require(source.datatype == sourceType,
      s"SelfPicker: $this doesn't support picking originated data of type: ${source.datatype}")
    source
  }
}