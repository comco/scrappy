package com.github.comco.scrappy.picker

import scala.reflect.runtime.universe._

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
import com.github.comco.scrappy.Shape

/**
 * ZipPicker :: (a -> [b]) -> (a -> [c]) -> (a -> [(b, c)])
 */
case class ZipPicker[-A <: Shape.Any: TypeTag, +B <: Shape.Any: TypeTag, +C <: Shape.Any: TypeTag](
    firstPicker: Picker[A, Shape.Seq[B]],
    secondPicker: Picker[A, Shape.Seq[C]]) extends BasePicker[A, Shape.Seq[Shape.Tuple]] {
  require(firstPicker compatibleWith secondPicker, s"Arguments to ZipPicker should be compatible: $firstPicker and $secondPicker")

  lazy val sourceType = {
    val r = firstPicker.sourceType
    r
  }

  lazy val tupleType = TupleType(firstPicker.targetType.elementType, secondPicker.targetType.elementType)
  lazy val targetType = SeqType(tupleType)

  def doPickData(source: Data[A]): SeqData[Shape.Tuple] = {
    val firstResult = firstPicker.pickData(source)
    val secondResult = secondPicker.pickData(source)
    return SeqData(targetType, firstResult.elements.zip(secondResult.elements).map {
      case (a, b) => TupleData(a, b)
    })
  }

  def doPickOriginatedData(source: OriginatedData[A]): OriginatedSeqData[Shape.Tuple] = {
    val firstResult = firstPicker.pickOriginatedData(source)
    val secondResult = secondPicker.pickOriginatedData(source)
    val origin = source.origin.computedWithTargetType(targetType)
    val elements = firstResult.elements.zip(secondResult.elements).zipWithIndex.map {
      case ((a, b), i) => {
        OriginatedTupleData(tupleType, origin.append(ElementStep(targetType, i)), IndexedSeq(a, b))
      }
    }
    return ???
  }
}