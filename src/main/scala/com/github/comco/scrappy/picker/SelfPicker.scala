package com.github.comco.scrappy.picker

import scala.reflect.runtime.universe._

import com.github.comco.scrappy.Type
import com.github.comco.scrappy.data.Data
import com.github.comco.scrappy.originated_data.OriginatedData
import com.github.comco.scrappy.Shape

/**
 * Identity picker - picks itself.
 */
case class SelfPicker[SelfShape <: Shape.Any: TypeTag](val sourceType: Type[SelfShape])
    extends Picker[SelfShape, SelfShape] {
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