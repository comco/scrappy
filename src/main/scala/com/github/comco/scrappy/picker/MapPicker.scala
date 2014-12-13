package com.github.comco.scrappy.picker

import com.github.comco.scrappy.SeqType
import com.github.comco.scrappy.data.SeqData
import com.github.comco.scrappy.originated_data.OriginatedSeqData

case class MapPicker(val f: Picker) extends BaseSeqPicker {
  def sourceType = SeqType(f.sourceType)
  def targetType = SeqType(f.targetType)

  def doPickData(source: SeqData) =
    SeqData(targetType, source.elements.map(f.pickData(_)))

  def doPickOriginatedData(source: OriginatedSeqData) = {
    val pickedData = doPickData(source.data)
    OriginatedSeqData(
      pickedData,
      source.origin.computedWithTargetType(pickedData.datatype),
      source.elements.map(f.pickOriginatedData(_)))
  }
}