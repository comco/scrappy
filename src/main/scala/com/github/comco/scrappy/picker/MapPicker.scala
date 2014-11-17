package com.github.comco.scrappy.picker

import com.github.comco.scrappy.SeqType
import com.github.comco.scrappy.data.SeqData
import com.github.comco.scrappy.originated_data.OriginatedSeqData
import com.github.comco.scrappy.data.Data
import com.github.comco.scrappy.Type

case class MapPicker(val f: Picker[Type[Any], Type[Nothing]])
    extends BasePicker[SeqType, SeqType] {

  override def sourceType = SeqType(f.sourceType)
  override def targetType = SeqType(f.targetType)

  override def doPickData(source: Data[SeqType]) =
    SeqData(targetType, source.elements.map(f.pickData(_)))

  override def doPickOriginatedData(source: OriginatedSeqData) = {
    val pickedData = doPickData(source.data)
    OriginatedSeqData(
      pickedData,
      source.origin.computedWithTargetType(pickedData.datatype),
      source.elements.map(f.pickOriginatedData(_)))
  }
}