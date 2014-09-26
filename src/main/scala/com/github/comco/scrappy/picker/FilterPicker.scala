package com.github.comco.scrappy.picker

import com.github.comco.scrappy.PrimitiveType
import com.github.comco.scrappy.SeqType
import com.github.comco.scrappy.data.Data
import com.github.comco.scrappy.data.PrimitiveData
import com.github.comco.scrappy.data.SeqData
import com.github.comco.scrappy.originated_data.OriginatedSeqData

case class FilterPicker(val cond: Picker) extends BaseSeqPicker {
  require(cond.targetType == PrimitiveType.BooleanPrimitiveType,
      s"FilterPicker argument: $cond should be a boolean picker.")

  def sourceType = SeqType(cond.sourceType)
  def targetType = sourceType

  def check(data: Data): Boolean =
    cond.pickData(data).asInstanceOf[PrimitiveData[Boolean]].value

  def doPickData(source: SeqData) =
    SeqData(sourceType, source.elements.filter(check))

  def doPickOriginatedData(source: OriginatedSeqData) = {
    OriginatedSeqData(
      doPickData(source.data),
      source.origin.computed,
      source.elements.filter {
        d => check(d.data)
      })
  }
}