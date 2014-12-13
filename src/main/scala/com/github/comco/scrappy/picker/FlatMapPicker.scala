package com.github.comco.scrappy.picker

import com.github.comco.scrappy.SeqType
import com.github.comco.scrappy.data.SeqData
import com.github.comco.scrappy.originated_data.OriginatedSeqData

/**
 * A FlatMapPicker; f : a -> [b]; flatMap(f) : [a] -> [b] = [a].map(f).flatten
 */
case class FlatMapPicker(val f: Picker) extends BaseSeqPicker {
  require(f.targetType.isInstanceOf[SeqType], s"FlatMapPicker needs f to have a sequence type, not: $f")

  def sourceType = SeqType(f.sourceType)
  def targetType = f.targetType.asInstanceOf[SeqType]

  def doPickData(source: SeqData): SeqData = {
    SeqData(targetType, source.elements.flatMap(f.pickData(_).asInstanceOf[SeqData].elements))
  }

  def doPickOriginatedData(source: OriginatedSeqData): OriginatedSeqData = {
    val pickedData = doPickData(source.data)
    val origin = source.origin.computedWithTargetType(targetType)
    val elements = source.elements.flatMap(f.pickOriginatedData(_).asInstanceOf[OriginatedSeqData].elements)
    OriginatedSeqData(
      pickedData.datatype,
      origin,
      elements)
  }
}