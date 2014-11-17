package com.github.comco.scrappy.picker

import com.github.comco.scrappy.Type
import com.github.comco.scrappy.data.Data
import com.github.comco.scrappy.originated_data.OriginatedData
import com.github.comco.scrappy.data.SeqData
import com.github.comco.scrappy.originated_data.OriginatedSeqData
import com.github.comco.scrappy.SeqType

/**
 * Represents data transformations. A picker instance works both on bare data
 * and on originated data.
 */
abstract class Picker[-SourceType >: Type.Nil <: Type.Any, +TargetType >: Type.Nil <: Type.Any] {
  def sourceType: Type.Any
  def accepts(tpe: SourceType): Boolean = {
    tpe <:< sourceType
  }
  
  def targetType: TargetType

  def pickData(source: Data[SourceType]): Data[TargetType]
  def pickOriginatedData(source: OriginatedData[SourceType]): OriginatedData[TargetType]
  
  def compatibleWith(that: Picker[Type.Any, Type.Nil]): Boolean = {
    that.accepts(this.targetType)
  }
}