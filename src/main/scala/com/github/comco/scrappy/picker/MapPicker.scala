package com.github.comco.scrappy.picker

import com.github.comco.scrappy.SeqType
import com.github.comco.scrappy.Type
import com.github.comco.scrappy.data.Data
import com.github.comco.scrappy.data.Data.Data_To_SeqData
import com.github.comco.scrappy.data.SeqData
import com.github.comco.scrappy.originated_data.OriginatedData
import com.github.comco.scrappy.originated_data.OriginatedData.OriginatedData_To_OriginatedSeqData
import com.github.comco.scrappy.originated_data.OriginatedSeqData

case class MapPicker(val f: Picker[Type.Any, Type.Nil])
    extends BasePicker[SeqType, SeqType] {

  override def sourceType = SeqType(f.sourceType)
  override def targetType = SeqType(f.targetType)

  override def doPickData(source: Data.Seq) =
    SeqData(targetType, source.elements.map(f.pickData(_)))

  override def doPickOriginatedData(source: OriginatedData.Seq) = {
    val pickedData = doPickData(source.data)
    OriginatedSeqData(
      pickedData,
      source.origin.computedWithTargetType(pickedData.datatype),
      source.elements.map(f.pickOriginatedData(_)))
  }
}