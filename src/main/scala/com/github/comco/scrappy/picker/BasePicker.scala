package com.github.comco.scrappy.picker

import com.github.comco.scrappy.data.Data
import com.github.comco.scrappy.originated_data.OriginatedData
import com.github.comco.scrappy.Type

/**
 * Base class for general pickers.
 */
abstract class BasePicker[SourceType >: Type.Nil <: Type.Any, TargetType >: Type.Nil <: Type.Any]
    extends Picker[SourceType, TargetType] {

  override def pickData(source: Data[SourceType]): Data[TargetType] = {
    require(source.datatype <:< sourceType)
    doPickData(source)
  } ensuring (_.datatype <:< targetType)

  override def pickOriginatedData(source: OriginatedData[SourceType]): OriginatedData[TargetType] = {
    require(source.datatype <:< sourceType)
    doPickOriginatedData(source)
  } ensuring (_.datatype <:< targetType)

  def doPickData(source: Data[SourceType]): Data[TargetType]

  def doPickOriginatedData(source: OriginatedData[SourceType]): OriginatedData[TargetType]
}