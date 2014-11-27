package com.github.comco.scrappy.picker

import scala.reflect.runtime.universe._

import com.github.comco.scrappy.SeqType
import com.github.comco.scrappy.Type
import com.github.comco.scrappy.data.Data
import com.github.comco.scrappy.data.SeqData
import com.github.comco.scrappy.originated_data.OriginatedData
import com.github.comco.scrappy.originated_data.OriginatedSeqData
import com.github.comco.scrappy.Shape

case class MapPicker[-SourceShape <: Shape.Any: TypeTag, +TargetShape <: Shape.Any: TypeTag](
  val f: Picker[SourceShape, TargetShape])
    extends BasePicker[Shape.Seq[SourceShape], Shape.Seq[TargetShape]] {

  def sourceType = SeqType(f.sourceType)
  def targetType = SeqType(f.targetType)

  override def doPickData(source: Data.Seq[SourceShape]): SeqData[TargetShape] = {
    val targetElements = source.elements.map(f.pickData(_))
    SeqData(targetType, targetElements)
  }

  override def doPickOriginatedData(source: OriginatedData.Seq[SourceShape]) = {
    val pickedData = doPickData(source.data)
    OriginatedSeqData.computed(
      pickedData,
      source.origin.computedWithTargetType(pickedData.datatype),
      source.elements.map(f.pickOriginatedData(_)))
  }
}