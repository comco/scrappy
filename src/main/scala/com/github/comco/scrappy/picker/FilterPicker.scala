package com.github.comco.scrappy.picker

import scala.reflect.runtime.universe._

import com.github.comco.scrappy.PrimitiveType
import com.github.comco.scrappy.SeqType
import com.github.comco.scrappy.data.Data
import com.github.comco.scrappy.data.PrimitiveData
import com.github.comco.scrappy.data.SeqData
import com.github.comco.scrappy.originated_data.OriginatedSeqData
import com.github.comco.scrappy.Type
import com.github.comco.scrappy.originated_data.OriginatedData
import com.github.comco.scrappy.Shape

case class FilterPicker[SourceShape <: Shape.Any: TypeTag](val cond: Picker[SourceShape, Shape.Primitive[Boolean]])
    extends Picker[Shape.Seq[SourceShape], Shape.Seq[SourceShape]] {

  def sourceType = SeqType(cond.sourceType)
  def targetType = SeqType(cond.sourceType.asInstanceOf[Type[SourceShape]])

  def doPickData(source: Data.Seq[SourceShape]) =
    SeqData(sourceType, source.elements.filter(cond.pickData(_).raw))

  def doPickOriginatedData(source: OriginatedData.Seq[SourceShape]) =
    OriginatedSeqData.computed(doPickData(source.data),
      source.origin.computed,
      source.elements.filter(cond.pickOriginatedData(_).data.raw))
}