package com.github.comco.scrappy.picker

import com.github.comco.scrappy.PrimitiveType
import com.github.comco.scrappy.SeqType
import com.github.comco.scrappy.data.Data
import com.github.comco.scrappy.data.PrimitiveData
import com.github.comco.scrappy.data.SeqData
import com.github.comco.scrappy.originated_data.OriginatedSeqData
import com.github.comco.scrappy.Type
import com.github.comco.scrappy.originated_data.OriginatedData

case class FilterPicker[SourceType >: Type.Nil <: Type.Any](val cond: Picker[SourceType, Type.Primitive[Boolean]])
    extends Picker[Type.Seq, Type.Seq] {
  
  def sourceType = SeqType(cond.sourceType)
  def targetType = sourceType

  def check(data: Data.Any): Boolean =
    cond.pickData(data.asInstanceOf[Data[SourceType]]).value

  def doPickData(source: Data[SeqType]) =
    SeqData(sourceType, source.elements.filter(check))

  def doPickOriginatedData(source: OriginatedData[SeqType]) = {
    OriginatedSeqData(
      doPickData(source.data),
      source.origin.computed,
      source.elements.filter {
        d => check(d.data)
      })
  }
}