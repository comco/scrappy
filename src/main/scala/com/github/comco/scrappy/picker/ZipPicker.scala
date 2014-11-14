package com.github.comco.scrappy.picker

import com.github.comco.scrappy.BotType
import com.github.comco.scrappy.SeqType
import com.github.comco.scrappy.TupleType
import com.github.comco.scrappy.Type
import com.github.comco.scrappy.data.Data
import com.github.comco.scrappy.data.SeqData
import com.github.comco.scrappy.data.TupleData
import com.github.comco.scrappy.originated_data.OriginatedSeqData
import com.github.comco.scrappy.originated_data.OriginatedData
import com.github.comco.scrappy.originated_data.OriginatedTupleData
import com.github.comco.scrappy.pointer.ElementStep

case class ZipPicker(firstPicker: Picker, secondPicker: Picker) extends BasePicker {
  require(firstPicker compatibleWith secondPicker, s"Arguments to ZipPicker should be compatible: $firstPicker and $secondPicker")
  require(firstPicker.targetType.isInstanceOf[SeqType] && secondPicker.targetType.isInstanceOf[SeqType],
    s"For construction of a ZipPicker, both pickers: $firstPicker and $secondPicker need to be SeqType-s.")

  lazy val sourceType: Type = {
    val r = firstPicker.sourceType.meet(secondPicker.sourceType)
    require(r != BotType, s"ZipPicker needs compatible pickers. The passed pickers: $firstPicker and $secondPicker are incompatible.")
    r
  }

  def firstTargetType = firstPicker.targetType.asInstanceOf[SeqType]
  def secondTargetType = secondPicker.targetType.asInstanceOf[SeqType]

  lazy val tupleType = TupleType(firstTargetType.elementType, secondTargetType.elementType)
  lazy val targetType: SeqType = SeqType(tupleType)

  def doPickData(source: Data): SeqData = {
    val firstResult = firstPicker.pickData(source).asInstanceOf[SeqData]
    val secondResult = secondPicker.pickData(source).asInstanceOf[SeqData]
    return SeqData(targetType, firstResult.elements.zip(secondResult.elements).map {
      case (a, b) => TupleData(tupleType)(a, b)
    })
  }

  def doPickOriginatedData(source: OriginatedData): OriginatedSeqData = {
    val firstResult = firstPicker.pickOriginatedData(source).asInstanceOf[OriginatedSeqData]
    val secondResult = secondPicker.pickOriginatedData(source).asInstanceOf[OriginatedSeqData]
    val origin = source.origin.computedWithTargetType(targetType)
    val elements = firstResult.elements.zip(secondResult.elements).zipWithIndex.map {
      case ((a, b), i) => {
        OriginatedTupleData(tupleType, origin.append(ElementStep(targetType, i)), IndexedSeq(a, b))
      }
    }
    return OriginatedSeqData(targetType, origin, elements)
  }
}