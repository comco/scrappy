package com.github.comco.scrappy.picker

import com.github.comco.scrappy.Type
import com.github.comco.scrappy.data.Data
import com.github.comco.scrappy.originated_data.OriginatedData
import com.github.comco.scrappy.TopType

/**
 * Discards its source (leaving only the origin) and returns a constant.
 */
case class ConstPicker[B](val data: Data[Type[B]])
    extends Picker[Type[Nothing], Type[B]] {
  val targetType = data.datatype

  def doPickData(source: Data[Type[Nothing]]) = data

  def doPickOriginatedData(source: OriginatedData[Type[Nothing]]) =
    OriginatedData.from(data,
      source.origin.computedWithTargetType(targetType))
}