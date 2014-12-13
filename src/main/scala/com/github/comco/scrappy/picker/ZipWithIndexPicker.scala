package com.github.comco.scrappy.picker

import com.github.comco.scrappy.SeqType
import com.github.comco.scrappy.Implicits._
import com.github.comco.scrappy.data.Data
import com.github.comco.scrappy.data.SeqData
import com.github.comco.scrappy.data.PrimitiveData
import com.github.comco.scrappy.originated_data.OriginatedSeqData
import com.github.comco.scrappy.originated_data.OriginatedData
import com.github.comco.scrappy.pointer.ElementStep
import com.github.comco.scrappy.originated_data.OriginatedTupleData
import com.github.comco.scrappy.originated_data.OriginatedPrimitiveData
import com.github.comco.scrappy.pointer.CoordinateStep

// f : a -> [b] and zipWithIndex(f) : a -> [(b, Int)]
case class ZipWithIndexPicker(f: Picker) extends BasePicker {
  require(f.targetType.isInstanceOf[SeqType], s"Argument to ZipWithIndexPicker should return a SeqType: $f")
  val sourceType = f.sourceType
  val fTargetType = f.targetType.asInstanceOf[SeqType]

  val tupleType = tuple(fTargetType.elementType, int)
  val targetType = seq(tupleType)

  def doPickData(source: Data): SeqData = {
    val fres = f.pickData(source).asInstanceOf[SeqData]
    return seq(fres.elements.zipWithIndex.map {
      case (e, i) => tuple(e, PrimitiveData(i))
    }: _*)
  }

  def doPickOriginatedData(source: OriginatedData): OriginatedSeqData = {
    val fResult = f.pickOriginatedData(source).asInstanceOf[OriginatedSeqData]
    val origin = source.origin.computedWithTargetType(targetType)
    val elements = fResult.elements.zipWithIndex.map {
      case (e, i) => {
        val tupleOrigin = origin.append(ElementStep(targetType, i))
        val originatedI = OriginatedPrimitiveData(i, tupleOrigin.append(CoordinateStep(tupleType, 1)))
        OriginatedTupleData(tupleType, tupleOrigin, IndexedSeq(e, originatedI))
      }
    }
    return OriginatedSeqData(targetType, origin, elements)
  }
}