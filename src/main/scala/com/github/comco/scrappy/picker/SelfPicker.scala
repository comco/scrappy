package com.github.comco.scrappy.picker

import com.github.comco.scrappy._

/**
 * Identity picker - picks itself.
 */
case class SelfPicker[+Shape <: Shape.Any](val sourceType: Type[Shape]) extends Picker[Shape, Shape] {
  def targetType = sourceType

  def pickData(source: Data[Shape]) = {
    require(source.datatype == sourceType,
      s"SelfPicker: $this doesn't support picking data of type: ${source.datatype}")
    source
  }

  def pickOriginatedData(source: OriginatedData[Shape]) = {
    require(source.datatype == sourceType,
      s"SelfPicker: $this doesn't support picking originated data of type: ${source.datatype}")
    source
  }
}