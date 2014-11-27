package com.github.comco.scrappy.picker

import scala.reflect.runtime.universe._

import com.github.comco.scrappy.data.Data
import com.github.comco.scrappy.originated_data.OriginatedData
import com.github.comco.scrappy.Type
import com.github.comco.scrappy.Shape

/**
 * Base class for general pickers.
 */
abstract class BasePicker[-SourceShape <: Shape.Any: TypeTag, +TargetShape <: Shape.Any: TypeTag]
    extends Picker[SourceShape, TargetShape] {

  def pickData(source: Data[SourceShape]): Data[TargetShape] = {
    require(source.datatype <:< sourceType)
    doPickData(source)
  } ensuring (_.datatype <:< targetType)

  def pickOriginatedData(source: OriginatedData[SourceShape]): OriginatedData[TargetShape] = {
    require(source.datatype <:< sourceType)
    doPickOriginatedData(source)
  } ensuring (_.datatype <:< targetType)

  def doPickData(source: Data[SourceShape]): Data[TargetShape]

  def doPickOriginatedData(source: OriginatedData[SourceShape]): OriginatedData[TargetShape]
}