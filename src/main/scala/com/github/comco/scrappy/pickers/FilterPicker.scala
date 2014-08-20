package com.github.comco.scrappy.pickers

import com.github.comco.scrappy._

case class FilterPicker(val cond: Picker) extends BaseSeqPicker {
  require(cond.targetType == PrimitiveType.BooleanPrimitiveType)

  def sourceType = SeqType(cond.sourceType)
  def targetType = sourceType

  def check(data: DataDomain.Data): Boolean =
    cond.pickData(data).asInstanceOf[DataDomain.PrimitiveData[Boolean]].value

  def doPickData(source: DataDomain.SeqData) =
    DataDomain.SeqData(sourceType, source.elements.filter(check))

  def doPickOriginatedData(source: OriginatedDataDomain.SeqData) = {
    OriginatedDataDomain.ComputedSeqData(
      doPickData(source.data),
      source.origin.computed,
      source.elements.filter {
        d => check(d.data)
      })
  }
}