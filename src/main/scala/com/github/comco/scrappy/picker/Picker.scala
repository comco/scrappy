package com.github.comco.scrappy.picker

import scala.reflect.runtime.universe._

import com.github.comco.scrappy.Type
import com.github.comco.scrappy.data.Data
import com.github.comco.scrappy.originated_data.OriginatedData
import com.github.comco.scrappy.data.SeqData
import com.github.comco.scrappy.originated_data.OriginatedSeqData
import com.github.comco.scrappy.SeqType
import com.github.comco.scrappy.Shape

/**
 * Represents data transformations. A picker instance works both on bare data
 * and on originated data.
 */
abstract class Picker[-SourceShape <: Shape.Any: TypeTag, +TargetShape <: Shape.Any: TypeTag] {
  type SourceType = Type[SourceShape]
  type TargetType = Type[TargetShape]

  def sourceType: Type.Any

  def accepts(tpe: Type.Any): Boolean = {
    tpe <:< sourceType
  }

  def targetType: TargetType

  def pickData(source: Data[SourceShape]): Data[TargetShape]
  def pickOriginatedData(source: OriginatedData[SourceShape]): OriginatedData[TargetShape]

  def compatibleWith(that: Picker.Any): Boolean = {
    that.accepts(this.targetType)
  }
}

object Picker {
  type Any = Picker[Shape.Nil, Shape.Any]
}