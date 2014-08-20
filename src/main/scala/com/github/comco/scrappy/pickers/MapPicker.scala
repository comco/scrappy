package com.github.comco.scrappy.pickers

import com.github.comco.scrappy._

case class MapPicker(val f: Picker) extends BaseSeqPicker {
  def sourceType = SeqType(f.sourceType)
  def targetType = SeqType(f.targetType)

  def doPickData(source: DataDomain.SeqData) =
    DataDomain.SeqData(targetType, source.elements.map(f.pickData(_)))

  def doPickOriginatedData(source: OriginatedDataDomain.SeqData) =
    OriginatedDataDomain.ComputedSeqData(
      doPickData(source.data),
      source.origin.computed,
      source.elements.map(f.pickOriginatedData(_)))
}