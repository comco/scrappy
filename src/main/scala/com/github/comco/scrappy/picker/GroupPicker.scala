package com.github.comco.scrappy.picker

import scala.reflect.runtime.universe._

import scala.IndexedSeq
import com.github.comco.scrappy.SeqType
import com.github.comco.scrappy.TupleType
import com.github.comco.scrappy.data.SeqData
import com.github.comco.scrappy.data.TupleData
import com.github.comco.scrappy.originated_data.OriginatedData
import com.github.comco.scrappy.originated_data.OriginatedSeqData
import com.github.comco.scrappy.originated_data.OriginatedTupleData
import com.github.comco.scrappy.Shape
import com.github.comco.scrappy.data.Data

/**
 * Groups a sequence of objects into groups based on a picker.
 * group(by : a -> b) : [a] -> [(b, [a])]
 */
case class GroupPicker[-A <: Shape.Any: TypeTag, +B <: Shape.Any: TypeTag](by: Picker[A, B])
    extends BasePicker[Shape.Seq[A], Shape.Seq[Shape.Tuple]] {
  def sourceType = SeqType(by.sourceType)
  def tupleType = TupleType(by.targetType, sourceType)
  def targetType = SeqType(tupleType)

  def doPickData(source: Data.Seq[A]): SeqData[Shape.Tuple] = {
    val rawResultMap = source.elements.groupBy(by.pickData(_))
    val results = rawResultMap.toSeq.map {
      case (value, elements) =>
        TupleData(tupleType,
          IndexedSeq(value, SeqData(sourceType, elements)))
    }
    SeqData(targetType, results)
  }

  def doPickOriginatedData(source: OriginatedData.Seq[A]): OriginatedSeqData[Shape.Tuple] = {
    ???
  }
}