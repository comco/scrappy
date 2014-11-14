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

case class ZipPicker(firstPicker: Picker with Picker.ReturningSeq,
    secondPicker: Picker with Picker.ReturningSeq) extends BasePicker {
  require(firstPicker compatibleWith secondPicker, s"Arguments to ZipPicker should be compatible: $firstPicker and $secondPicker")

  lazy val sourceType: Type = {
    val r = firstPicker.sourceType.meet(secondPicker.sourceType)
    require(r != BotType, s"ZipPicker needs compatible pickers. The passed pickers: $firstPicker and $secondPicker are incompatible.")
    r
  }

  def firstTargetType = firstPicker.targetType
  def secondTargetType = secondPicker.targetType

  lazy val tupleType = TupleType(firstTargetType.elementType, secondTargetType.elementType)
  lazy val targetType: SeqType = SeqType(tupleType)

  def doPickData(source: Data): SeqData = {
    val firstResult = firstPicker.pickData(source)
    val secondResult = secondPicker.pickData(source)
    return SeqData(targetType, firstResult.elements.zip(secondResult.elements).map {
      case (a, b) => TupleData(tupleType)(a, b)
    })
  }

  def doPickOriginatedData(source: OriginatedData): OriginatedSeqData = {
    val firstResult = firstPicker.pickOriginatedData(source)
    val secondResult = secondPicker.pickOriginatedData(source)
    val origin = source.origin.computedWithTargetType(targetType)
    val elements = firstResult.elements.zip(secondResult.elements).zipWithIndex.map {
      case ((a, b), i) => {
        OriginatedTupleData(tupleType, origin.append(ElementStep(targetType, i)), IndexedSeq(a, b))
      }
    }
    return OriginatedSeqData(targetType, origin, elements)
  }
}