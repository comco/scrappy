package com.github.comco.scrappy.pickers

import com.github.comco.scrappy._

case class FilterPicker(val cond: Picker) extends BaseSeqPicker {
  require(cond.targetType == PrimitiveType.BooleanPrimitiveType)

  def sourceType = SeqType(cond.sourceType)
  def targetType = sourceType

  def check(data: DataDomain.Data): Boolean =
    cond.pick(data).asInstanceOf[DataDomain.PrimitiveData[Boolean]].value

  def doPick(source: DataDomain.SeqData) =
    DataDomain.SeqData(sourceType, source.elements.filter(check))

  def doPickWithOrigin(source: OriginatedDataDomain.SeqData) = {
    OriginatedDataDomain.ComputedSeqData(
      doPick(source.data),
      source.origin.computed,
      source.elements.filter {
        d => check(d.data)
      })
  }
}